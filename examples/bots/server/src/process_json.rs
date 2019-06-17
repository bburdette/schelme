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

#[derive(Serialize, Deserialize)]
pub struct Model { 
  name: String, 
  triangles: Value,
}

fn load_model(name: &str) -> Result<ServerResponse, Error> { 
  Ok(ServerResponse { 
      what: "model".to_string(),
      content: serde_json::from_str(util::load_string(format!("models/{}", name).as_str())?.as_str())?
    })
}

fn load_stl(name: &str) -> Result<Vec<u8>, Error> { 
    // put the filename in there, padded to 40 chars with spaces.
    let mut f = File::open(format!("models/stl/{}", name).as_str())?;
    let mut buffer = Vec::new();

    // read the whole file
    f.read_to_end(&mut buffer)?;
 
    Ok(buffer)
}


fn save_model(name: &str, data: &Value) -> Result<ServerResponse, Error> { 
  util::write_string(format!("models/{}", name).as_str(), data.to_string().as_str())?;

  Ok(ServerResponse { 
      what: "model written!".to_string(),
      content: serde_json::Value::Null,
    })
}

// public json msgs don't require login.
pub fn process_public_json( ip: &Option<&str>, msg: PublicMessage) -> Result<Option<ServerResponse>, Error> {
  match msg.what.as_str() {
    "getmodel" => {
      match msg.data {
        None => Ok(Some(ServerResponse {
          what: "no model specified!".to_string(),
          content: serde_json::Value::Null,
        })),
        Some(data) => {
          info!("public getmodel:{}", data);
          match data.to_string().as_str() {
            // filenames hardcoded to prevent accessing random files on the filesystem.
            "\"treble-clef\"" => (load_model("treble-clef")).map(Some),
            "\"bass-clef\"" => (load_model("bass-clef")).map(Some),
            "\"sharp\"" => (load_model("sharp")).map(Some),
            "\"flat\"" => (load_model("flat")).map(Some),
            "\"natural\"" => (load_model("natural")).map(Some),
            wat => Err(failure::err_msg(format!("invalid 'model' code:'{}'", wat))),
          }
          }
        }
      }
    "stats" => {
      match msg.data {
        None => Ok(None),
        Some(data) => {
          info!("stats! {:?}, {}", ip, data);
          Ok(None)
          }
        }
      }

    // get Meta Tag Base = getmtb
    wat => Err(failure::err_msg(format!("invalid 'what' code:'{}'", wat))),
  }
}


// public json msgs don't require login.
pub fn process_model_json( msg: PublicMessage) -> Result<Vec<u8>, Error> {
  match msg.what.as_str() {
    "getstl" => {
      match msg.data {
        None => Err(failure::err_msg("no model specified!")),
        Some(data) => {
          info!("model getstl:{}", data);
          match data.to_string().as_str() {
            // filenames hardcoded to prevent accessing arbitrary filesystem files.
            "\"0.stl\"" => load_stl("0.stl"),
            "\"1.stl\"" => load_stl("1.stl"),
            "\"2.stl\"" => load_stl("2.stl"),
            "\"3.stl\"" => load_stl("3.stl"),
            "\"4.stl\"" => load_stl("4.stl"),
            "\"5.stl\"" => load_stl("5.stl"),
            "\"6.stl\"" => load_stl("6.stl"),
            "\"7.stl\"" => load_stl("7.stl"),
            "\"8.stl\"" => load_stl("8.stl"),
            "\"9.stl\"" => load_stl("9.stl"),
            "\"add.stl\"" => load_stl("add.stl"),
            "\"a.stl\"" => load_stl("a.stl"),
            "\"A.stl\"" => load_stl("A.stl"),
            "\"aug.stl\"" => load_stl("aug.stl"),
            "\"B.stl\"" => load_stl("B.stl"),
            "\"C.stl\"" => load_stl("C.stl"),
            "\"D.stl\"" => load_stl("D.stl"),
            "\"E.stl\"" => load_stl("E.stl"),
            "\"F.stl\"" => load_stl("F.stl"),
            "\"G.stl\"" => load_stl("G.stl"),
            "\"maj.stl\"" => load_stl("maj.stl"),
            "\"min.stl\"" => load_stl("min.stl"),
            "\"m.stl\"" => load_stl("m.stl"),
            "\"M.stl\"" => load_stl("M.stl"),
            "\"sharp.stl\"" => load_stl("sharp.stl"),
            "\"-.stl\"" => load_stl("-.stl"),
            "\"+.stl\"" => load_stl("+.stl"),
            "\"u.stl\"" => load_stl("u.stl"),
            "\"bass-clef.stl\"" => load_stl("bass-clef.stl"),
            "\"c-clef.stl\"" => load_stl("c-clef.stl"),
            "\"treble-clef.stl\"" => load_stl("treble-clef.stl"),
            "\"flat.stl\"" => load_stl("flat.stl"),
            "\"natural.stl\"" => load_stl("natural.stl"),
            wat => Err(failure::err_msg(format!("invalid 'model' code:'{}'", wat))),
          }
          }
        }
      }
    // get Meta Tag Base = getmtb
    wat => Err(failure::err_msg(format!("invalid 'what' code:'{}'", wat))),
  }
}


