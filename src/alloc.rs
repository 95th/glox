use std::alloc::{AllocErr, AllocInit, AllocRef, Global, Layout, MemoryBlock, ReallocPlacement};
use std::cell::RefCell;
use std::ptr::NonNull;

pub use alloc_wg::string::String;
pub use alloc_wg::vec::Vec;

pub trait Alloc: AllocRef + Copy {
    fn allocated(&self) -> usize;
}

pub struct LoxAlloc {
    allocated: RefCell<usize>,
}

impl LoxAlloc {
    pub fn new() -> Self {
        Self {
            allocated: RefCell::new(0),
        }
    }
}

impl Alloc for &LoxAlloc {
    fn allocated(&self) -> usize {
        *self.allocated.borrow()
    }
}

unsafe impl AllocRef for &LoxAlloc {
    fn alloc(
        &mut self,
        layout: Layout,
        init: AllocInit,
    ) -> Result<MemoryBlock, std::alloc::AllocErr> {
        trace!(
            "Allocating : {}, current allocated: {}",
            layout.size(),
            self.allocated()
        );
        *self.allocated.borrow_mut() += layout.size();
        Global.alloc(layout, init)
    }

    unsafe fn dealloc(&mut self, ptr: NonNull<u8>, layout: Layout) {
        trace!(
            "Freeing : {}, current allocated: {}",
            layout.size(),
            self.allocated()
        );
        *self.allocated.borrow_mut() -= layout.size();
        Global.dealloc(ptr, layout)
    }

    unsafe fn grow(
        &mut self,
        ptr: NonNull<u8>,
        layout: Layout,
        new_size: usize,
        placement: ReallocPlacement,
        init: AllocInit,
    ) -> Result<MemoryBlock, AllocErr> {
        trace!(
            "Growing from {} to {}, current allocated: {}",
            layout.size(),
            new_size,
            self.allocated()
        );
        *self.allocated.borrow_mut() += new_size - layout.size();
        Global.grow(ptr, layout, new_size, placement, init)
    }

    unsafe fn shrink(
        &mut self,
        ptr: NonNull<u8>,
        layout: Layout,
        new_size: usize,
        placement: ReallocPlacement,
    ) -> Result<MemoryBlock, AllocErr> {
        trace!(
            "Shrinking from {} to {}, current allocated: {}",
            layout.size(),
            new_size,
            self.allocated()
        );
        *self.allocated.borrow_mut() -= layout.size() - new_size;
        Global.shrink(ptr, layout, new_size, placement)
    }
}
