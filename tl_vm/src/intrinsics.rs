use std::sync::{Arc, RwLock};

use tl_util::Rf;

pub trait IntrinsicType {}

pub trait IntrinsicUpcast {
    fn upcast(self) -> Rf<dyn IntrinsicType + Send + Sync>;
}

impl<T: IntrinsicType + Send + Sync + 'static> IntrinsicUpcast for Rf<T> {
    fn upcast(self) -> Rf<dyn IntrinsicType + Send + Sync> {
        Rf(self.0 as Arc<RwLock<_>>)
    }
}
