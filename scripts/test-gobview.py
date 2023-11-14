# needs preinstalled libraries:
# pip3 install selenium webdriver-manager

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from threading import Thread
import subprocess

PORT = 8080 # has to match port defined in goblint_http.ml
DIRECTORY = "run"
IP = "localhost"
url = 'http://' + IP + ':' + str(PORT) + '/'

# cleanup
def cleanup(browser, thread):
  print("cleanup")
  browser.close()
  p.kill()
  thread.join()

# serve GobView in different thread so it does not block the testing
def serve():
  global p
  goblint_http_path = '_build/default/gobview/goblint-http-server/goblint_http.exe'
  p = subprocess.Popen(['./' + goblint_http_path,
                  '-with-goblint', '../GobContextGas/goblint',
                  '-goblint', '--set', 'files[+]', '"../GobContextGas/tests/regression/00-sanity/01-assert.c"'])

print("serving at port", PORT)
thread = Thread(target=serve, args=())
thread.start()

# installation of browser
print("starting installation of browser\n")
options = Options()
options.add_argument('headless')
browser = webdriver.Chrome(service=Service(ChromeDriverManager().install()),options=options)
print("finished webdriver installation \n")
browser.maximize_window()
browser.implicitly_wait(10);

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

    cleanup(browser, thread)

except Exception as e:
    cleanup(browser, thread)
    raise e
