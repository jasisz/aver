use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub struct StandardHttpProvider;

pub const STANDARD_HTTP_NATIVE_IDENTITY: &str = "aver.standard.Http/native";
pub const STANDARD_HTTP_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

#[cfg(not(feature = "http"))]
fn unavailable(operation: &str) -> ProviderFault {
    ProviderFault::new(
        "binding_unavailable",
        format!("{operation}: HTTP effects are not available in this build"),
    )
}

#[cfg(feature = "http")]
fn headers_from_provider(value: &ProviderValue) -> Result<crate::HttpHeaders, ProviderFault> {
    let ProviderValue::Map(entries) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            "Http headers must be Map<String, List<String>>",
        ));
    };
    let mut headers = crate::HttpHeaders::default();
    for (name, values) in entries {
        let ProviderValue::String(name) = name else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                "Http header names must be Strings",
            ));
        };
        let ProviderValue::List(values) = values else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                "Http header values must be List<String>",
            ));
        };
        let mut converted = Vec::with_capacity(values.len());
        for value in values {
            let ProviderValue::String(value) = value else {
                return Err(ProviderFault::new(
                    "invalid_arguments",
                    "Http header values must be Strings",
                ));
            };
            converted.push(crate::AverStr::from(value.as_str()));
        }
        headers = headers.insert_owned(
            crate::AverStr::from(name.to_ascii_lowercase()),
            crate::AverList::from_vec(converted),
        );
    }
    Ok(headers)
}

#[cfg(feature = "http")]
fn response_value(response: crate::HttpResponse) -> ProviderValue {
    let headers = response
        .headers
        .iter()
        .map(|(name, values)| {
            (
                ProviderValue::String(name.to_string()),
                ProviderValue::List(
                    values
                        .iter()
                        .map(|value| ProviderValue::String(value.to_string()))
                        .collect(),
                ),
            )
        })
        .collect();
    ProviderValue::Record {
        type_name: "Http.Response".to_string(),
        fields: vec![
            (
                "status".to_string(),
                ProviderValue::Int(response.status.into()),
            ),
            (
                "body".to_string(),
                ProviderValue::String(response.body.to_string()),
            ),
            ("headers".to_string(), ProviderValue::Map(headers)),
        ],
    }
}

#[cfg(feature = "http")]
fn result_value(result: Result<crate::HttpResponse, String>) -> ProviderValue {
    match result {
        Ok(response) => ProviderValue::ResultOk(Box::new(response_value(response))),
        Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
    }
}

impl CapabilityProvider for StandardHttpProvider {
    fn identity(&self) -> &str {
        STANDARD_HTTP_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_HTTP_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        #[cfg(not(feature = "http"))]
        {
            let _ = args;
            return Err(unavailable(&context.operation));
        }
        #[cfg(feature = "http")]
        match (context.operation.as_str(), args) {
            ("Http.get", [ProviderValue::String(url)]) => Ok(result_value(crate::http::get(url))),
            ("Http.head", [ProviderValue::String(url)]) => Ok(result_value(crate::http::head(url))),
            ("Http.delete", [ProviderValue::String(url)]) => {
                Ok(result_value(crate::http::delete(url)))
            }
            (
                operation @ ("Http.post" | "Http.put" | "Http.patch"),
                [
                    ProviderValue::String(url),
                    ProviderValue::String(body),
                    ProviderValue::String(content_type),
                    headers,
                ],
            ) => {
                let headers = headers_from_provider(headers)?;
                let result = match operation {
                    "Http.post" => crate::http::post(url, body, content_type, &headers),
                    "Http.put" => crate::http::put(url, body, content_type, &headers),
                    "Http.patch" => crate::http::patch(url, body, content_type, &headers),
                    _ => unreachable!(),
                };
                Ok(result_value(result))
            }
            (operation, _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} received an invalid argument shape"),
            )),
        }
    }
}
