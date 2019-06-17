extern crate serde_json;
use serde_json::Value;
use failure::Error;
use util;
use failure;
use std::fs::File;
use std::io::Read;

#[derive(Deserialize, Debug)]
pub struct PublicMessage {
  what: String,
  data: Option<serde_json::Value>,
}

#[derive(Deserialize, Debug)]
pub struct Message {
  pub uid: String,
  pwd: String,
  what: String,
  data: Option<serde_json::Value>,
}

#[derive(Serialize, Deserialize)]
pub struct ServerResponse {
  pub what: String,
  pub content: Value,
}


fn load_script(name: &str) -> Result<ServerResponse, Error> {
  Ok(ServerResponse { 
      what: "script".to_string(),
      content: serde_json::from_str(util::load_string(format!("scripts/{}", name).as_str())?.as_str())?
    })
}



fn save_script(name: &str, data: &Value) -> Result<ServerResponse, Error> { 
  util::write_string(format!("scripts/{}", name).as_str(), data.to_string().as_str())?;

  Ok(ServerResponse { 
      what: "script written!".to_string(),
      content: serde_json::Value::Null,
    })
}

// public json msgs don't require login.
pub fn process_public_json( ip: &Option<&str>, msg: PublicMessage) -> Result<Option<ServerResponse>, Error> {
  match msg.what.as_str() {
    "getscript" => {
      match msg.data {
        None => Ok(Some(ServerResponse {
          what: "no script specified!".to_string(),
          content: serde_json::Value::Null,
        })),
        Some(data) => {
          info!("public getscript:{}", data);
          match data.to_string().as_str() {
            name  => (load_script(name)).map(Some),
          }
          }
        }
      }

    // get Meta Tag Base = getmtb
    wat => Err(failure::err_msg(format!("invalid 'what' code:'{}'", wat))),
  }
}


