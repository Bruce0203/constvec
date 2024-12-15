#![feature(specialization)]
#![feature(const_copy_from_slice)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]

use core::slice;
use std::{
    fmt::{Debug, Pointer},
    mem::MaybeUninit,
    ops::Add,
};

const unsafe fn const_transmute<A, B>(a: A) -> B {
    if std::mem::size_of::<A>() != std::mem::size_of::<B>() {
        panic!("Size mismatch for generic_array::const_transmute");
    }

    #[repr(C)]
    union Union<A, B> {
        a: std::mem::ManuallyDrop<A>,
        b: std::mem::ManuallyDrop<B>,
    }

    let a = std::mem::ManuallyDrop::new(a);
    unsafe { std::mem::ManuallyDrop::into_inner(Union { a }.b) }
}

pub struct ConstVec<T> {
    len: usize,
    arr: T,
}

impl<T: Debug, const N: usize> Debug for ConstVec<[T; N]> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<T: PartialEq, const N: usize> PartialEq for ConstVec<[T; N]> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<T: Eq, const N: usize> Eq for ConstVec<[T; N]> {}

#[const_trait]
pub trait ConstEq {
    fn eq(&self, other: &Self) -> bool;
}

macro_rules! impl_const_eq {
    ($($type:ty),*) => {
        $(
        impl const ConstEq for $type {
            fn eq(&self, other: &Self) -> bool {
                *self == *other
            }
        }
        )*
    };
}

impl_const_eq!(
    u8, i8, u16, i16, u32, i32, u64, i64, f32, f64, bool, usize, isize, i128, u128
);

impl<T: const ConstEq, const N: usize> const ConstEq for ConstVec<[T; N]> {
    fn eq(&self, other: &Self) -> bool {
        let slice = self.as_slice();
        let rhs_slice = other.as_slice();
        if slice.len() != rhs_slice.len() {
            return false;
        }
        let mut i = 0;
        while i < slice.len() {
            if !slice[i].eq(&rhs_slice[i]) {
                return false;
            }
            i += 1;
        }
        true
    }
}

impl<T, const N: usize> ConstVec<[T; N]> {
    pub const fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(&self.arr as *const _ as *const T, self.len) }
    }

    pub const fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(&mut self.arr as *mut _ as *mut T, self.len) }
    }

    pub const fn new(len: usize, arr: [T; N]) -> Self {
        Self { len, arr }
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const unsafe fn set_len(&mut self, value: usize) {
        self.len = value;
    }

    pub const fn push(&mut self, value: &T)
    where
        [(); size_of::<T>()]:,
    {
        if self.len >= N {
            panic!("not enough remaining buffer");
        } else {
            let slice: &mut [[u8; size_of::<T>()]; N] = unsafe { const_transmute(&mut self.arr) };
            let value: &mut [u8; size_of::<T>()] = unsafe { const_transmute(value) };
            slice[self.len] = *value;
            self.len += 1;
        }
    }

    pub const fn append<const N2: usize>(&mut self, target: &ConstVec<[T; N2]>)
    where
        [(); size_of::<T>()]:,
    {
        if target.len() + self.len() >= N {
            panic!("not enough remaining buffer");
        } else {
            let mut i = 0;
            while i < target.len() {
                self.push(target.get(i));
                i += 1;
            }
        }
    }

    pub const fn clone(&self) -> Self
    where
        [(); size_of::<T>()]:,
    {
        #[allow(invalid_value)]
        let mut result: Self = unsafe { MaybeUninit::uninit().assume_init() };
        result.len = self.len;
        let dst: &mut [[u8; size_of::<T>()]; N] = unsafe { const_transmute(&mut result.arr) };
        let src: &[[u8; size_of::<T>()]; N] = unsafe { const_transmute(&self.arr) };
        dst.copy_from_slice(src);
        result
    }

    pub const fn pop_last(&mut self) -> &T {
        self.len -= 1;
        &self.arr[self.len]
    }

    pub const fn cutoff_front_at(&self, mid: usize) -> Self
    where
        [(); size_of::<T>()]:,
    {
        #[allow(invalid_value)]
        let mut result: Self = unsafe { MaybeUninit::uninit().assume_init() };
        result.len = self.len - mid;
        let dst: &mut [[u8; size_of::<T>()]; N] = unsafe { const_transmute(&mut result.arr) };
        let src: &[[u8; size_of::<T>()]; N] = unsafe { const_transmute(&self.arr) };
        let dst = unsafe {
            slice::from_raw_parts_mut(dst as *mut _ as *mut [u8; size_of::<T>()], result.len)
        };
        let src = unsafe {
            slice::from_raw_parts(
                (src as *const _ as *const T).add(mid) as *const [u8; size_of::<T>()],
                result.len,
            )
        };
        dst.copy_from_slice(src);
        result
    }

    pub const fn get(&self, index: usize) -> &T {
        if index >= self.len {
            panic!()
        }
        unsafe { &*(&self.arr as *const _ as *const T).add(index) }
    }

    pub const fn get_mut(&mut self, index: usize) -> &mut T {
        if index >= self.len {
            panic!()
        }
        unsafe { &mut *(&mut self.arr as *mut _ as *mut T).add(index) }
    }

    pub const unsafe fn set_with_no_drop_on_swap(&mut self, index: usize, value: &T)
    where
        [(); size_of::<T>()]:,
    {
        if index >= self.len {
            panic!()
        }
        unsafe { &mut *(&mut self.arr[index] as *mut _ as *mut [u8; size_of::<T>()]) }
            .copy_from_slice(unsafe {
                slice::from_raw_parts(value as *const _ as *const u8, size_of::<T>())
            });
    }

    pub const fn append_slice(&mut self, slice: &[T])
    where
        [(); size_of::<T>()]:,
    {
        let mut i = 0;
        while i < slice.len() {
            self.push(&slice[i]);
            i += 1;
        }
    }
}

impl<T: Clone> Clone for ConstVec<T> {
    fn clone(&self) -> Self {
        Self {
            len: self.len.clone(),
            arr: self.arr.clone(),
        }
    }
}

impl<T: Sized + Copy, const N1: usize, const N2: usize> const Add<&ConstVec<[T; N2]>>
    for &ConstVec<[T; N1]>
where
    [(); size_of::<T>()]:,
    [(); N1 + N2]:,
{
    type Output = ConstVec<[T; N1 + N2]>;

    fn add(self, rhs: &ConstVec<[T; N2]>) -> Self::Output {
        let slice1 = self;
        let slice2 = rhs;
        let mut slice: [[u8; size_of::<T>()]; N1 + N2] =
            unsafe { MaybeUninit::uninit().assume_init() };
        {
            let mut i = 0;
            while i < slice1.len {
                slice[i] = unsafe { const_transmute(slice1.arr[i]) };
                i += 1;
            }
        }
        {
            let mut i = 0;
            while i < slice2.len {
                slice[i + slice1.len] = unsafe { const_transmute(slice2.arr[i]) };
                i += 1;
            }
        }
        ConstVec {
            len: slice1.len + slice2.len,
            arr: unsafe { const_transmute(slice) },
        }
    }
}

#[cfg(test)]
mod test {
    use std::time::Instant;

    use crate::ConstVec;

    #[test]
    fn asdf() {
        std::thread::Builder::new()
            .stack_size(1024 * 1024 * 1000)
            .spawn(|| {
                let now = Instant::now();
                const fn asd() {
                    let mut vec = ConstVec::new(0, [0; 100000]);
                    vec.clone();
                    vec.append(&vec.clone());
                }
                asd();
                println!("{:?}", now.elapsed());
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
