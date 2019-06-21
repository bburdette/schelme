
function storeVal( [name, val] ) { 
  localStorage.setItem(name, val); 
  console.log("js storeVal storing " + name + ", " + val);
}

function getVal( [forstr, name] ) { 
  console.log("js getVal getting " + name + "," + localStorage.getItem(name) + " for " + forstr);
  app.ports.localVal.send([forstr, name,localStorage.getItem(name)]); 
}

function clearStorage () {
  console.log("clearstorage");
  localStorage.clear();
}
