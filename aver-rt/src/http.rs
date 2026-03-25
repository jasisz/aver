use crate::{AverList, AverStr, Header, HttpResponse};

pub fn get(url: &str) -> Result<HttpResponse, String> {
    simple_request("GET", url)
}

pub fn head(url: &str) -> Result<HttpResponse, String> {
    simple_request("HEAD", url)
}

pub fn delete(url: &str) -> Result<HttpResponse, String> {
    simple_request("DELETE", url)
}

pub fn post(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &AverList<Header>,
) -> Result<HttpResponse, String> {
    body_request("POST", url, body, content_type, headers)
}

pub fn put(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &AverList<Header>,
) -> Result<HttpResponse, String> {
    body_request("PUT", url, body, content_type, headers)
}

pub fn patch(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &AverList<Header>,
) -> Result<HttpResponse, String> {
    body_request("PATCH", url, body, content_type, headers)
}

fn simple_request(method: &str, url: &str) -> Result<HttpResponse, String> {
    let result = ureq::request(method, url)
        .timeout(std::time::Duration::from_secs(10))
        .call();
    response_value(result)
}

fn body_request(
    method: &str,
    url: &str,
    body: &str,
    content_type: &str,
    headers: &AverList<Header>,
) -> Result<HttpResponse, String> {
    let mut req = ureq::request(method, url)
        .timeout(std::time::Duration::from_secs(10))
        .set("Content-Type", content_type);
    for h in headers {
        req = req.set(&h.name, &h.value);
    }
    response_value(req.send_string(body))
}

fn response_value(result: Result<ureq::Response, ureq::Error>) -> Result<HttpResponse, String> {
    match result {
        Ok(resp) => build_response(resp),
        Err(ureq::Error::Status(_, resp)) => build_response(resp),
        Err(ureq::Error::Transport(e)) => Err(e.to_string()),
    }
}

fn build_response(resp: ureq::Response) -> Result<HttpResponse, String> {
    use std::io::Read;

    const BODY_LIMIT: u64 = 10 * 1024 * 1024;

    let status = resp.status() as i64;
    let header_names = resp.headers_names();
    let headers: Vec<Header> = header_names
        .iter()
        .map(|name| Header {
            name: AverStr::from(name.as_str()),
            value: AverStr::from(resp.header(name).unwrap_or("")),
        })
        .collect();

    let mut buf = Vec::new();
    let bytes_read = resp
        .into_reader()
        .take(BODY_LIMIT + 1)
        .read_to_end(&mut buf)
        .map_err(|e| format!("Http: failed to read response body: {}", e))?;
    if bytes_read as u64 > BODY_LIMIT {
        return Err("Http: response body exceeds 10 MB limit".to_string());
    }
    let body = AverStr::from(String::from_utf8_lossy(&buf).into_owned());
    Ok(HttpResponse {
        status,
        body,
        headers: AverList::from_vec(headers),
    })
}
