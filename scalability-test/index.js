const puppeteer = require('puppeteer');

const pairingCodeSelector = '.pairing-code';
const buttonSelector = '.primary.button';
const pinCodeSelector = 'input[name=pincode]';
const healthCenterSelector = "//button[contains(text(), 'Nyange Health Center')]";
const baseUrl = 'http://localhost:3000';

(async () => {

  function delay(time) {
    return new Promise(function (resolve) {
      setTimeout(resolve, time)
    });
  }

  async function login(page) {
    const cliArgs = process.argv.slice(2);
    const pairingCode = String(cliArgs[0]);
    const pinCode = String(cliArgs[1]);

    if (!pairingCode || !pinCode) {
      throw 'Usage: node index.js pairingCode pinCode';
    }

    await page.goto(baseUrl, {waitUntil: 'networkidle2'});
    // Wait until the element with "Enter pairing code" text becomes visible.
    await page.waitForSelector(pairingCodeSelector);
    // Fills the pairing code element and then, submit the form.
    await page.type(pairingCodeSelector, pairingCode);
    await page.click(buttonSelector);
    await page.waitForSelector(pinCodeSelector);
    await page.type(pinCodeSelector, pinCode);
    await page.waitForSelector('.primary.button:not([disabled])')
    await delay(500);
    await page.click(buttonSelector);
  }

  async function syncDevice(page) {
    await page.goto(baseUrl, {waitUntil: 'networkidle2'});
    await page.waitForSelector('.sync-icon');
    await page.click('.sync-icon');

    await delay(500);
    const elHandleArray = await page.$$('.health-centers button')

    elHandleArray.forEach(async el => {
      await el.click();
    });

    await delay(500);
  }

  async function selectHealthCenter(page) {
    await page.screenshot({path: 'hcselect.png', fullPage: true, captureBeyondViewport: true});
    // Wait until the element with "Nyange Health Center" becomes visible.
    await page.waitForXPath(healthCenterSelector);
    // Clicks on the element using XPath.
    const elements = await page.$x(healthCenterSelector);
    await elements[0].click();
    await page.waitForSelector('.sync-icon');
  }

  async function recordPatient(page) {
    await page.reload();
    await page.goto(baseUrl + '/#person/directory/new', {waitUntil: 'networkidle2'});

    // Fill textfields with random value.
    const inputs = await page.$$('.registration-form input')
    inputs.forEach(async el => {
      await el.type((Math.random() + 1).toString(36).substring(7));
    });

    // Choose select items randomly.
    for (let i = 0; i < 10; i++) {
      await page.evaluate(() => {
        let selects = document.getElementsByTagName('select');
        for (let i = 0; i < selects.length; i++) {
          if (selects[i].getAttribute('disabled')) {
            continue;
          }
          let items = selects[i].getElementsByTagName('option');
          selects[i].selectedIndex = Math.min(Math.floor((Math.random() * items.length)) + 1, items.length - 1);
          selects[i].dispatchEvent(new Event('change'));
        }
      });
      await delay(100);
    }

    // Set birthdate to a random date in the format of DD-MM-YYYY.
    await page.$x('//div/fieldset[1]/div[5]/div[2]/div/div/input')[0].click();
    await delay(100);
    await page.click('.date-selector--scrollable-year div:nth-child(1) li');
    await delay(100);
    await page.click('.date-selector--scrollable-year div:nth-child(2) li');
    await delay(100);
    await page.click('.date-selector--scrollable-year tbody td');
    await delay(100);
    await page.screenshot({path: 'date.png', fullPage: true, captureBeyondViewport: true});

    await page.click('input[value="male"]');

    await page.screenshot({path: 'reg.png', fullPage: true, captureBeyondViewport: true});

    await page.click(buttonSelector);
    await page.screenshot({path: 'regwitherrors.png', fullPage: true, captureBeyondViewport: true});
  }

  const browser = await puppeteer.launch();
  // It is helpful for the local tests when we execute it repeatedly.
  const context = await browser.createIncognitoBrowserContext();
  const page = await context.newPage();
  page.setViewport({width: 1920, height: 1024});

  await login(page);
  await selectHealthCenter(page);
  await syncDevice(page);
  await recordPatient(page);

  await browser.close();
})();
