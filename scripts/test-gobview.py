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
browser = webdriver.Chrome(service=Service(ChromeDriverManager().install()),options=Options())
print("finished webdriver installation \n")
browser.maximize_window()

# register cleanup function
# atexit.register(cleanup, browser, httpd, thread)

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

    cleanup(browser, httpd, thread)

except Exception as e:
    cleanup(browser, httpd, thread)
    raise e
