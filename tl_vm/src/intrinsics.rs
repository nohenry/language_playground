use std::sync::{Arc, RwLock};

use tl_util::Rf;

pub trait IntrinsicType: IntrinsicTypeClone {}

pub trait IntrinsicTypeClone {
    fn clone_intrinsic(&self) -> Box<dyn IntrinsicType + Send + Sync>;
}

impl<T> IntrinsicTypeClone for T
where
    T: 'static + IntrinsicType + Send + Sync + Clone,
{
    fn clone_intrinsic(&self) -> Box<dyn IntrinsicType + Send + Sync> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn IntrinsicType + Send + Sync> {
    fn clone(&self) -> Box<dyn IntrinsicType + Send + Sync> {
        self.clone_intrinsic()
    }
}

pub trait IntrinsicUpcast {
    fn upcast(self) -> Rf<dyn IntrinsicType + Send + Sync>;
}

impl<T: IntrinsicType + Send + Sync + 'static> IntrinsicUpcast for Rf<T> {
    fn upcast(self) -> Rf<dyn IntrinsicType + Send + Sync> {
        Rf(self.0 as Arc<RwLock<_>>)
    }
}
