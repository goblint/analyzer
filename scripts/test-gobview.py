# needs preinstalled libraries:
# pip3 install selenium webdriver-manager

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from threading import Thread
import http.server
import socketserver

PORT = 9000
DIRECTORY = "run"
IP = "localhost"
url = 'http://' + IP + ':' + str(PORT) + '/'

# cleanup
def cleanup(browser, httpd, thread):
  print("cleanup")
  browser.close()
  httpd.shutdown()
  httpd.server_close()
  thread.join()

# serve GobView in different thread so it does not block the testing
class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=DIRECTORY, **kwargs)
class Server(socketserver.TCPServer):
    allow_reuse_address = True # avoids that during a consecutive run the server cannot connect due to an 'Adress already in use' os error

httpd = Server((IP, PORT), Handler)
print("serving at port", PORT)
thread = Thread(target=httpd.serve_forever, args=())
thread.start()

# installation of browser
print("starting installation of browser\n")
options = Options()
options.add_argument('headless')
browser = webdriver.Chrome(service=Service(ChromeDriverManager().install()),options=options)
print("finished webdriver installation \n")
browser.maximize_window()

try:
    # retrieve and wait until page is fully loaded and rendered
    browser.get(url)
    print("open local GobView page\n")

    # check for the right page title:
    title = browser.title
    assert(title == "GobView")
    print("found the site's title", title)

    # check the general structure of the page (whether main element, navbar, left and right sidebar, content view and panel exists)
    # find_element throws an NoSuchElementException if this is not the case
    main = browser.find_element(By.CLASS_NAME, "main")
    leftS = browser.find_element(By.CLASS_NAME, "sidebar-left")
    rightS = browser.find_element(By.CLASS_NAME, "sidebar-right")
    content = browser.find_element(By.CLASS_NAME, "content")
    panel = browser.find_element(By.CLASS_NAME, "panel")
    print("found DOM elements main, sidebar-left, sidebar-right, content and panel")

    # test syntactic search
    leftS.find_element(By.LINK_TEXT,  "Search").click()
    leftS.find_element(By.CLASS_NAME, "switch-to-json").click()
    textfield = leftS.find_element(By.CLASS_NAME, "form-control")
    textfield.clear()
    textfield.send_keys('{"kind":["var"],"target":["name","fail"],"find":["uses"],"mode":["Must"]}')
    leftS.find_element(By.CLASS_NAME, "exec-button").click()
    results = leftS.find_elements(By.CLASS_NAME, "list-group-item")
    locations = []
    for r in results:
        for tr in r.find_elements(By.TAG_NAME, "tr"):
            if tr.find_element(By.TAG_NAME, "th").text == "Location":
                locations.insert(0,tr.find_element(By.TAG_NAME, "td").find_element(By.TAG_NAME, "a").text)

    print("syntactic search for variable use of 'fail' found", len(results), "results")
    for l in locations:
        print(l)
    assert(len(results) == 2)
    assert("tests/regression/00-sanity/01-assert.c:7" in locations)
    assert("tests/regression/00-sanity/01-assert.c:12" in locations)

    # clear results
    leftS.find_element(By.CLASS_NAME, "clear-btn").click()

    # test semantic search 1
    textfield = leftS.find_element(By.CLASS_NAME, "form-control")
    textfield.clear()
    textfield.send_keys('{"kind":["var"],"target":["name","success"],"find":["uses"],"expression":"success == 1","mode":["Must"]}')
    leftS.find_element(By.CLASS_NAME, "exec-button").click()
    results = leftS.find_elements(By.CLASS_NAME, "list-group-item")
    locations = []
    for r in results:
        for tr in r.find_elements(By.TAG_NAME, "tr"):
            if tr.find_element(By.TAG_NAME, "th").text == "Location":
                locations.insert(0,tr.find_element(By.TAG_NAME, "td").find_element(By.TAG_NAME, "a").text)

    print("semantic search for variable use of 'success' where it must be 1 found", len(results), "results")
    for l in locations:
        print(l)
    assert(len(results) == 2)
    assert("tests/regression/00-sanity/01-assert.c:10" in locations)
    assert("tests/regression/00-sanity/01-assert.c:5" in locations)

    # clear results
    leftS.find_element(By.CLASS_NAME, "clear-btn").click()

    # test semantic search 2
    textfield = leftS.find_element(By.CLASS_NAME, "form-control")
    textfield.clear()
    textfield.send_keys('{"kind":["var"],"target":["name","success"],"find":["uses"],"expression":"success == 0","mode":["Must"]}')
    leftS.find_element(By.CLASS_NAME, "exec-button").click()
    results = leftS.find_elements(By.CLASS_NAME, "list-group-item")
    locations = []
    for r in results:
        for tr in r.find_elements(By.TAG_NAME, "tr"):
            if tr.find_element(By.TAG_NAME, "th").text == "Location":
                locations.insert(0,tr.find_element(By.TAG_NAME, "td").find_element(By.TAG_NAME, "a").text)

    print("semantic search for variable use of 'success' where it must be 0 found", len(results), "results")
    for l in locations:
        print(l)
    assert(len(results) == 0)

    # close "No results found" alert
    leftS.find_element(By.CLASS_NAME, "btn-close").click()

    cleanup(browser, httpd, thread)

except Exception as e:
    cleanup(browser, httpd, thread)
    raise e
