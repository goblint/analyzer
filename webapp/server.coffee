express = require("express")
path = require("path")
fs = require("fs")
eyes = require("eyes")
xml2js = require("xml2js")
xmlParser = new xml2js.Parser()
sys = require("sys")
exec = require("child_process").exec
util = require("util")
app = express()

# configure server
app.configure ->
  # app.use(express.logger());
  app.set "port", process.env.PORT or 3000
  app.set "views", __dirname + ""
  app.use express.bodyParser() # needed for req.files
  app.use express.methodOverride() # hidden input _method for put/del
  # app.use require("stylus").middleware(__dirname + "/public")
  app.use require('connect-assets')()
  app.use express.static(__dirname + "/public")

app.configure "development", ->
  app.use express.errorHandler()


# configure paths
srcPath = __dirname + "/../tests/regression/17-file/"

# routes
app.get "/", (req, res) ->
  fs.readdir srcPath, (err, files) ->
    res.render "index.jade",
      pageTitle: "Goblint"
      files: files

app.get "/source/:file", (req, res) ->
  file = path.join(srcPath, req.params.file)
  console.log "reading ", file
  (fs.createReadStream file).pipe res # fast, streaming, no whitespaces

app.get "/result/:file", (req, res) ->
  file = path.join(srcPath, req.params.file)
  cmd = "../goblint --sets result pretty "+file
  exec cmd, (error, stdout, stderr) ->
    sys.print "stderr:", stderr
    res.send stdout


app.listen app.get("port")
