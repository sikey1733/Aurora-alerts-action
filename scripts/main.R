name: Run Space Weather Alert

on:
  schedule:
    - cron: "0 8 * * *"  # запускать каждый день в 08:00 UTC 
  workflow_dispatch:     

jobs:
  run-r-script:
    runs-on: ubuntu-latest

    env:
      TELEGRAM_TOKEN: ${{ secrets.TELEGRAM_TOKEN }}
      TELEGRAM_CHAT_ID: ${{ secrets.TELEGRAM_CHAT_ID }}

    steps:
    - name: Checkout repo
      uses: actions/checkout@v3

    - name: Set up R
      uses: r-lib/actions/setup-r@v2

    - name: Install dependencies
      run: |
        R -e 'install.packages(c("httr", "jsonlite", "tidyverse"), repos="https://cloud.r-project.org")'

    - name: Run script
      run: |
        Rscript scripts/main.R
