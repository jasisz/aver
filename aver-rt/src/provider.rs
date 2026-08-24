//! Transport-neutral values and calls for in-process capability providers.
//!
//! This API deliberately does not expose the Aver VM's `Value` or
//! `NanValue`. Providers see a closed first-order value tree and opaque
//! resource payloads. A future IPC or Component Model adapter can therefore
//! preserve this boundary without inheriting VM representation details.

mod codec;
mod disk;
mod process;
mod runtime;
mod tcp;

use std::any::Any;
use std::collections::BTreeSet;
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};

use crate::AverInt;

pub use codec::{ProviderCodec, provider_value_order_key};
pub use disk::{STANDARD_DISK_FINGERPRINT, STANDARD_DISK_NATIVE_IDENTITY, StandardDiskProvider};
pub use process::{
    STANDARD_PROCESS_FINGERPRINT, STANDARD_PROCESS_NATIVE_IDENTITY, StandardProcessProvider,
    standard_process_stop_requested,
};
pub use runtime::{
    NativeProviderRegistry, STANDARD_TIME_FINGERPRINT, STANDARD_TIME_NATIVE_IDENTITY,
    StandardTimeProvider, standard_time_now, standard_time_sleep, standard_time_unix_ms,
};
#[cfg(feature = "random")]
pub use runtime::{
    STANDARD_RANDOM_FINGERPRINT, STANDARD_RANDOM_NATIVE_IDENTITY, StandardRandomProvider,
    standard_random_float, standard_random_int,
};
pub use tcp::{STANDARD_TCP_FINGERPRINT, STANDARD_TCP_NATIVE_IDENTITY, StandardTcpProvider};

/// A host-owned payload carried by a capability resource.
///
/// Only providers can create one. Aver programs receive an opaque handle; the
/// language runtime tags that handle with its binding and declared resource
/// type before it can cross another provider call.
#[derive(Clone)]
pub struct ProviderResource {
    id: u64,
    payload: Arc<dyn Any + Send + Sync>,
}

impl ProviderResource {
    pub fn new<T: Any + Send + Sync>(payload: T) -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        Self {
            id: NEXT_ID.fetch_add(1, AtomicOrdering::Relaxed),
            payload: Arc::new(payload),
        }
    }

    /// Provider-visible identity. It is process-local and never serialized.
    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.payload.downcast_ref()
    }
}

impl fmt::Debug for ProviderResource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ProviderResource(<opaque>)")
    }
}

/// Values legal at a capability provider boundary.
#[derive(Debug, Clone)]
pub enum ProviderValue {
    Int(AverInt),
    Float(f64),
    String(String),
    /// Canonical octets carried by Aver's nominal `Bytes` refinement.
    /// The source record layout never crosses the provider boundary.
    Bytes(Vec<u8>),
    Bool(bool),
    Unit,
    Tuple(Vec<ProviderValue>),
    List(Vec<ProviderValue>),
    Vector(Vec<ProviderValue>),
    Map(Vec<(ProviderValue, ProviderValue)>),
    ResultOk(Box<ProviderValue>),
    ResultErr(Box<ProviderValue>),
    OptionSome(Box<ProviderValue>),
    OptionNone,
    Record {
        type_name: String,
        fields: Vec<(String, ProviderValue)>,
    },
    Variant {
        type_name: String,
        variant: String,
        fields: Vec<ProviderValue>,
    },
    Resource(ProviderResource),
}

impl ProviderValue {
    /// Coarse runtime shape used in boundary diagnostics. Values and resource
    /// identities are deliberately omitted.
    pub fn shape(&self) -> String {
        match self {
            Self::Int(_) => "Int".to_string(),
            Self::Float(_) => "Float".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bytes(_) => "Bytes".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::Unit => "Unit".to_string(),
            Self::Tuple(values) => format!("Tuple({})", values.len()),
            Self::List(_) => "List".to_string(),
            Self::Vector(_) => "Vector".to_string(),
            Self::Map(_) => "Map".to_string(),
            Self::ResultOk(_) => "Result.Ok".to_string(),
            Self::ResultErr(_) => "Result.Err".to_string(),
            Self::OptionSome(_) => "Option.Some".to_string(),
            Self::OptionNone => "Option.None".to_string(),
            Self::Record { type_name, .. } => format!("record {type_name}"),
            Self::Variant {
                type_name, variant, ..
            } => format!("variant {type_name}.{variant}"),
            Self::Resource(_) => "capability resource".to_string(),
        }
    }
}

/// Stable metadata supplied to every provider invocation.
#[derive(Debug, Clone)]
pub struct ProviderContext {
    pub capability: String,
    pub operation: String,
    pub contract_hash: String,
    pub model_hash: String,
}

/// Failure of the binding or host transport itself.
///
/// An Aver-declared `Result.Err` is represented by
/// [`ProviderValue::ResultErr`] and is not a `ProviderFault`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderFault {
    pub code: String,
    pub message: String,
}

impl ProviderFault {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }
}

impl fmt::Display for ProviderFault {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.code, self.message)
    }
}

impl std::error::Error for ProviderFault {}

/// Synchronous, first-order provider contract for phase 1.
///
/// Implementations must own any serialization they require. Aver may call a
/// provider concurrently from branches of an independent product, hence the
/// `Send + Sync` bound. A serial provider can take its own lock in `invoke`.
pub trait CapabilityProvider: Send + Sync {
    /// Human-readable implementation identity recorded in replay provenance.
    fn identity(&self) -> &str;

    /// Deployment/build fingerprint. It is provenance, not part of the
    /// capability theorem or contract hash.
    fn fingerprint(&self) -> &str;

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault>;
}

/// One host implementation pinned to one exact capability contract.
///
/// This type lives in `aver-rt` so the same binding value can be installed in
/// an embedded bytecode VM or a generated Rust artifact. Language-specific
/// values and VM arena identities deliberately do not appear here.
#[derive(Clone)]
pub struct ProviderBinding {
    id: u64,
    capability: String,
    contract_hash: String,
    operations: BTreeSet<String>,
    provider: Arc<dyn CapabilityProvider>,
}

impl ProviderBinding {
    pub fn new(
        capability: impl Into<String>,
        contract_hash: impl Into<String>,
        operations: impl IntoIterator<Item = impl Into<String>>,
        provider: Arc<dyn CapabilityProvider>,
    ) -> Self {
        static NEXT_BINDING_ID: AtomicU64 = AtomicU64::new(1);
        Self {
            id: NEXT_BINDING_ID.fetch_add(1, AtomicOrdering::Relaxed),
            capability: capability.into(),
            contract_hash: contract_hash.into(),
            operations: operations.into_iter().map(Into::into).collect(),
            provider,
        }
    }

    pub fn capability(&self) -> &str {
        &self.capability
    }

    pub fn contract_hash(&self) -> &str {
        &self.contract_hash
    }

    pub fn operations(&self) -> &BTreeSet<String> {
        &self.operations
    }

    pub fn provider_identity(&self) -> &str {
        self.provider.identity()
    }

    pub fn provider_fingerprint(&self) -> &str {
        self.provider.fingerprint()
    }

    /// Process-local binding identity used to validate capability-resource
    /// handles. It is runtime metadata and must never be serialized.
    pub fn runtime_id(&self) -> u64 {
        self.id
    }
}

/// Runtime metadata required to validate a native provider binding.
///
/// Type layouts stay in the language/generated adapters; the native core only
/// needs stable identities and the complete operation set. This keeps
/// `aver-rt` independent of the compiler's `Type` representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderContractSpec {
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub operations: BTreeSet<String>,
}

/// Transport-neutral replay identity for one installed native binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeProviderProvenance {
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub provider: String,
    pub fingerprint: String,
}

/// Declared contract plus the optional live binding currently installed for
/// it. Recorded replay validates theorem identity without a live provider;
/// pure/reissued replay additionally pins the deployment identity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeProviderContractProvenance {
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub provider: Option<String>,
    pub fingerprint: Option<String>,
}

impl ProviderContractSpec {
    pub fn new(
        capability: impl Into<String>,
        contract_hash: impl Into<String>,
        model_hash: impl Into<String>,
        operations: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Self {
            capability: capability.into(),
            contract_hash: contract_hash.into(),
            model_hash: model_hash.into(),
            operations: operations.into_iter().map(Into::into).collect(),
        }
    }
}

/// Opaque language-side reference to a provider-owned payload.
///
/// The payload itself remains in [`NativeProviderRegistry`]'s shared store.
/// The tuple is unforgeable through Aver source and validates binding, type,
/// slot, and generation at every live provider boundary.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ProviderResourceHandle {
    binding_id: u64,
    type_name: String,
    slot: u64,
    generation: u64,
}

impl fmt::Debug for ProviderResourceHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ProviderResourceHandle(<opaque>)")
    }
}

impl ProviderResourceHandle {
    pub fn binding_id(&self) -> u64 {
        self.binding_id
    }

    pub fn type_name(&self) -> &str {
        &self.type_name
    }

    pub fn slot(&self) -> u64 {
        self.slot
    }

    pub fn generation(&self) -> u64 {
        self.generation
    }

    pub fn from_runtime_parts(
        binding_id: u64,
        type_name: String,
        slot: u64,
        generation: u64,
    ) -> Self {
        Self {
            binding_id,
            type_name,
            slot,
            generation,
        }
    }
}
