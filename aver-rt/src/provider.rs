//! Transport-neutral values and calls for in-process capability providers.
//!
//! This API deliberately does not expose the Aver VM's `Value` or
//! `NanValue`. Providers see a closed first-order value tree and opaque
//! resource payloads. A future IPC or Component Model adapter can therefore
//! preserve this boundary without inheriting VM representation details.

use std::any::Any;
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::AverInt;

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
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
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

/// Standard native Time provider shared by the bytecode VM and generated
/// Rust artifacts. The VM selects it through a registry; native artifacts
/// bind the same adapter statically.
pub struct StandardTimeProvider;

pub const STANDARD_TIME_NATIVE_IDENTITY: &str = "aver.standard.Time/native";
pub const STANDARD_TIME_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

pub fn standard_time_now() -> String {
    crate::time_now()
}

pub fn standard_time_unix_ms() -> AverInt {
    AverInt::from_i64(crate::time_unix_ms())
}

pub fn standard_time_sleep(ms: &AverInt) -> Result<(), ProviderFault> {
    let ms = ms.to_i64().ok_or_else(|| {
        ProviderFault::new(
            "integer_out_of_range",
            "Time.sleep: ms must fit a 64-bit integer",
        )
    })?;
    if ms < 0 {
        return Err(ProviderFault::new(
            "negative_duration",
            "Time.sleep: ms must be non-negative",
        ));
    }
    crate::time_sleep(ms);
    Ok(())
}

impl CapabilityProvider for StandardTimeProvider {
    fn identity(&self) -> &str {
        STANDARD_TIME_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_TIME_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Time.now" if args.is_empty() => Ok(ProviderValue::String(standard_time_now())),
            "Time.unixMs" if args.is_empty() => Ok(ProviderValue::Int(standard_time_unix_ms())),
            "Time.sleep" => {
                let [ProviderValue::Int(ms)] = args else {
                    return Err(ProviderFault::new(
                        "invalid_arguments",
                        format!("Time.sleep expects one Int argument, got {}", args.len()),
                    ));
                };
                standard_time_sleep(ms)?;
                Ok(ProviderValue::Unit)
            }
            operation => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Time provider cannot invoke '{operation}'"),
            )),
        }
    }
}
