const puppeteer = require('puppeteer');

const pairingCodeSelector = '.pairing-code';
const buttonSelector = '.primary.button';
const pinCodeSelector = 'input[name=pincode]';
const healthCenterSelector = "//button[contains(text(), 'Nyange Health Center')]";
const baseUrl = 'http://localhost:3000';

const inputDelay = 550;
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
    await page.screenshot({path: '00.png', fullPage: true, captureBeyondViewport: true});
    // Fills the pairing code element and then, submit the form.
    await page.type(pairingCodeSelector, pairingCode);
    await page.click(buttonSelector);
    await page.waitForSelector(pinCodeSelector);
    await page.screenshot({path: '01.png', fullPage: true, captureBeyondViewport: true});
    await page.type(pinCodeSelector, pinCode);
    await page.screenshot({path: '02.png', fullPage: true, captureBeyondViewport: true});
    await delay(inputDelay);
    await page.screenshot({path: 'beforepin.png', fullPage: true, captureBeyondViewport: true});
    await page.waitForSelector('.primary.button:not([disabled])')
    await delay(inputDelay);
    await page.click(buttonSelector);
    await delay(inputDelay);
    await page.screenshot({path: 'after.png', fullPage: true, captureBeyondViewport: true});
  }

  async function syncDevice(page) {
    await page.goto(baseUrl, {waitUntil: 'networkidle2'});
    await page.waitForSelector('.sync-icon');
    await page.click('.sync-icon');

    await delay(inputDelay);
    const elHandleArray = await page.$$('.health-centers button')

    elHandleArray.forEach(async el => {
      await el.click();
    });

    await delay(inputDelay);
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

    // Phone number special case.
    let phoneNumber = await page.$x('//div/div[2]/div/div/div/fieldset[4]/div/div[2]/input');
    await phoneNumber[0].type('1234567890');
    await phoneNumber[0].tap();
    await page.screenshot({path: 'phonenum' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});

    // Person photo upload.
    let futureFileChooser = page.waitForFileChooser();
    await page.click('.dz-clickable');
    await page.screenshot({path: 'filechooser0-' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});
    let fileChooser = await futureFileChooser;
    await page.screenshot({path: 'filechooser1-' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});
    await fileChooser.accept(['person.jpg']);
    await page.screenshot({path: 'filechooser2-' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});

    // Fill textfields with random value.
    for (let i = 0; i < 20; i++) {
      const inputs = await page.$$('.registration-form input')
      inputs.forEach(async elInput => {
        const type = await page.evaluate(el => el.getAttribute("type"), elInput);
        const currentValue = await page.evaluate(el => el.value, elInput);
        if (type === 'number') {
          // National ID is not required.
          return;
        }
        else if (currentValue == null || currentValue.length < 1) {
          await elInput.type((Math.random() + 1).toString(36).substring(7), {delay: inputDelay});
        }
        delay(inputDelay);
      });
    }

    // Choose select items randomly.
    for (let i = 0; i < 20; i++) {
      await page.evaluate(() => {
        let selects = document.getElementsByTagName('select');
        for (let i = 0; i < selects.length; i++) {
          if (selects[i].getAttribute('disabled')) {
            continue;
          }
          let items = selects[i].getElementsByTagName('option');
          selects[i].selectedIndex = Math.min(Math.floor((Math.random() * items.length)) + 1, items.length - 1);
          if (selects[i].options[selects[i].selectedIndex].value === '') {
            selects[i].selectedIndex = Math.min(Math.floor((Math.random() * items.length)) + 1, items.length - 1);
          }
          selects[i].dispatchEvent(new Event('change'));
        }
      });
      await delay(inputDelay);
    }

    // Handle exceptions.
    await page.evaluate(() => {
      // Male checkbox.
      let radio = document.evaluate('//div[1]/div[2]/div/div[2]/div/div/div/fieldset[1]/div[7]/input[1]', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
      radio.checked = true;
      radio.dispatchEvent(new Event('change'));
    });
    await delay(inputDelay);

    // Set birthdate to a random date in the format of DD-MM-YYYY.
    let dateInput = await page.$x('//div/fieldset[1]/div[5]/div[2]/div/div/input');
    await dateInput[0].click();
    await delay(inputDelay);
    await page.click('.date-selector--scrollable-year div:nth-child(1) li');
    await delay(inputDelay);
    await page.click('.date-selector--scrollable-year div:nth-child(2) li');
    await delay(inputDelay);
    await page.click('.date-selector--scrollable-year tbody td');
    await delay(inputDelay);
    await page.screenshot({path: 'date' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});

    await page.screenshot({path: 'reg' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});

    await delay(3000);
    await page.click(buttonSelector);
    await page.screenshot({path: 'regwitherrors' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});
    await delay(3000);
    await page.click(buttonSelector);
    await page.screenshot({path: 'regwitherrors' + Date.now() + '.png', fullPage: true, captureBeyondViewport: true});
  }

  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox'],
  });

  // It is helpful for the local tests when we execute it repeatedly.
  const context = await browser.createIncognitoBrowserContext();
  const page = await context.newPage();
  page.setViewport({width: 1920, height: 1024});

  await login(page);
  await selectHealthCenter(page);
  await syncDevice(page);
  for (let i = 0; i < 50; i++) {
    await recordPatient(page);
    await syncDevice(page);
  }

  browser.close();
})();
