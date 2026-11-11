import os
from pathlib import Path
from selenium import webdriver
from selenium import webdriver # load packages
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.by import By

# Create a temporary directory accessible to Snap Firefox
tmp_dir = Path.home() / "snap/firefox/common/selenium-tmp"
tmp_dir.mkdir(parents=True, exist_ok=True)

# Equivalent to R's Sys.setenv(TMPDIR = ...)
os.environ["TMPDIR"] = str(tmp_dir)

# Use my installed geckodriver
service = Service("/home/sgrehl/miniconda3/envs/toolbox_env/bin/geckodriver")

# Tell the browser to run in headless mode (no GUI)
#options = Options()
#options.add_argument("-headless")

driver = webdriver.Firefox( # Start Firefox
    service=service#,
    #options=options
)


def wait_random(min_secs, max_secs):
  time.sleep(random.uniform(min_secs, max_secs))