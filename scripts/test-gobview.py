# needs preinstalled libraries:
# pip3 install selenium webdriver-manager

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
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

    # simulate work flow for analysis rerun
    parameterViewTab = browser.find_element(By.ID, "nav-item-2")
    parameterViewTab.click()
    parameterView = browser.find_element(By.ID, "parameterview")
    print("found DOM element parameterview")
    inputBar = browser.find_element(By.CLASS_NAME, "input")
    inputBar.clear()
    invalidFeedback = browser.find_element(By.X_PATH, '//div[@class="invalid-tooltip"]')
    
    feedback = invalidFeedback.text
    assert(feedback == "At least one parameter has to be entered")
    print("found the feedback", feedback)

    parameter = '--incremental.force-reanalyze.funs ["main"]'
    inputBar.send_keys(parameter)
    inputBar.send_keys(Keys.ENTER)
    
    # wait for ten seconds to let the analysis finish
    browser.implicitly_wait(10)

    parameterChip = browser.find_element(By.X_PATH, '//span[@class="m-1 badge rounded-pill bg-secondary text"]')
    textFromParameterChip = parameterChip.text
    assert(parameterChip == parameter)
    print("found the parameter chip in history", textFromParameterChip)

    # search for first tick symbol in history
    executedSvg = browser.find_element(By.X_PATH, '//svg[@class="bi bi-check2"]')
    print("found tick symbol in history indicating successful reanalysis")

    cleanup(browser, httpd, thread)

except Exception as e:
    cleanup(browser, httpd, thread)
    raise e
