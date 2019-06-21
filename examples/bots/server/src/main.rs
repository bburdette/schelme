extern crate actix;
extern crate actix_web;
extern crate crypto_hash;
extern crate env_logger;
extern crate failure;
extern crate futures;
extern crate lettre;
extern crate lettre_email;
extern crate openssl;
extern crate rand;
extern crate serde_json;
extern crate time;
extern crate toml;
extern crate uuid;
#[macro_use]
extern crate log;

use actix_web::fs::NamedFile;
use actix_web::http::{Method, StatusCode};
use actix_web::middleware::Logger;
use actix_web::Binary;
use actix_web::{
  http, server, App, AsyncResponder, HttpMessage, HttpRequest, HttpResponse, Responder, Result,
};
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

#[macro_use]
extern crate serde_derive;

use failure::Error;
use futures::future::Future;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
mod process_json;
mod util;
use process_json::{process_public_json, PublicMessage, ServerResponse};

fn files(req: &HttpRequest) -> Result<NamedFile> {
  let path: PathBuf = req.match_info().query("tail")?;
  info!("files: {:?}", path);
  let stpath = Path::new("static/").join(path);
  Ok(NamedFile::open(stpath)?)
}

fn favicon(req: &HttpRequest) -> Result<NamedFile> {
  let stpath = Path::new("static/favicon.ico");
  Ok(NamedFile::open(stpath)?)
}

fn sitemap(req: &HttpRequest) -> Result<NamedFile> {
  let stpath = Path::new("static/sitemap.txt");
  Ok(NamedFile::open(stpath)?)
}

/// simple index handler
fn mainpage(req: &HttpRequest) -> Result<HttpResponse> {
  info!(
    "remote ip: {:?}, request:{:?}",
    req.connection_info().remote(),
    req
  );

  match util::load_string("static/index.html") {
    Ok(s) => {
      // response
      Ok(
        HttpResponse::build(StatusCode::OK)
          .content_type("text/html; charset=utf-8")
          .body(s),
      )
    }
    Err(e) => Err(e.into()),
  }
}

fn public(req: &HttpRequest) -> Box<Future<Item = String, Error = Error>> {
  let ci = req.connection_info().clone();
  req
    .json()
    .from_err()
    .and_then(move |msg: PublicMessage| {
      Ok(
        match process_json::process_public_json(&(ci.remote()), msg) {
          Ok(sr) => match serde_json::to_string(&sr) {
            Ok(s) => s,
            Err(e) => e.to_string(),
          },
          Err(e) => {
            error!("uh oh, 'public' err: {:?}", e);
            let se = ServerResponse {
              what: "server error".to_string(),
              content: serde_json::Value::String(e.to_string()),
            };
            match serde_json::to_string(&se) {
              Ok(s) => s,
              Err(e) => e.to_string(),
            }
          }
        },
      )
    })
    .responder()
}

#[derive(Deserialize, Debug)]
struct Config {
  ip: String,
  port: u16,
  uid: Option<String>,
  pwd: Option<String>,
  tlskey: Option<String>,
  tlscerts: Option<String>,
  redirectport: Option<u16>,
  redirectdomain: Option<String>,
}

fn defcon() -> Config {
  Config {
    ip: "127.0.0.1".to_string(),
    port: 8000,
    uid: None,
    pwd: None,
    tlskey: None,
    tlscerts: None,
    redirectport: None,
    redirectdomain: None,
  }
}

fn load_config() -> Config {
  match util::load_string("config.toml") {
    Err(e) => {
      error!("error loading config.toml: {:?}", e);
      defcon()
    }
    Ok(config_str) => match toml::from_str(config_str.as_str()) {
      Ok(c) => c,
      Err(e) => {
        error!("error loading config.toml: {:?}", e);
        defcon()
      }
    },
  }
}

fn main() {
  let config = load_config();
  env_logger::init();

  info!("server init!");

  // load ssl keys
  let optbuilder = match (config.tlskey, config.tlscerts) {
    (Some(key), Some(certs)) => {
      let mut builder = SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
      builder.set_private_key_file(key, SslFiletype::PEM).unwrap();
      builder.set_certificate_chain_file(certs).unwrap();
      Some(builder)
    }
    _ => {
      warn!("tlskey, tlscerts not found; no tls for you!");
      None
    }
  };

  let sys = actix::System::new("schelme-bots");

  {
    let s = server::new(move || {
      App::new()
        .resource("/public", |r| r.method(Method::POST).f(public))
        .resource(r"/static/{tail:.*}", |r| r.method(Method::GET).f(files))
        .resource("/favicon.ico", |r| r.method(Method::GET).f(favicon))
        .resource("/sitemap.txt", |r| r.method(Method::GET).f(sitemap))
        .resource(r"/{tail:.*}", |r| r.method(Method::GET).f(mainpage))
    });

    match optbuilder {
      Some(builder) => s.bind_ssl(format!("{}:{}", config.ip, config.port), builder),
      None => s.bind(format!("{}:{}", config.ip, config.port)),
    }
  }
  .expect(format!("Can not bind to port {}", config.port).as_str())
  .start();

  // do some http redirecting?
  let _ = match (config.redirectport, config.redirectdomain) {
    (Some(port), Some(domain)) => Some(
      server::new(move || {
        App::with_state(domain.clone())
          .resource("{all:.*}", |r| {
            r.f(|r| {
              info!("redirect!{}", r.path());
              HttpResponse::Found()
                .header(
                  http::header::LOCATION,
                  format!("{}{}?{}", r.state(), r.path(), r.query_string()),
                )
                .finish()
            })
          })
          .middleware(Logger::default())
          .middleware(Logger::new("REDIRECTOR: %a %{User-Agent}i"))
      })
      .bind(format!("{}:{}", config.ip, port))
      .expect(format!("Can not bind to port {}", port).as_str())
      .start(),
    ),
    _ => None,
  };

  sys.run();
}
