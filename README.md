# campusdish-scraper

This program gets the food being served in the dinning hall at UofA from their website and displays it nicely.
You can get the different time periods individually or all at once

## Usage

Install cabal and run it with `cabal run campusdish-scraper <date offset> <period>`
where the period is `breakfast`, `lunch`, `dinner` or `all`.

for example `campusdish-scraper 1 dinner` would get dinner tommorow.
