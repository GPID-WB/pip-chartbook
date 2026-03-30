
// binned data source: https://datacatalog.worldbank.org/search/dataset/0064304/1000_binned_global_distribution
use "C:\Users\wb562356\OneDrive - WBG\Documents\PovcalNet related\PIP bins\20250930_2021_01_02_PROD\03-output\GlobalDist1000bins_1981_2025_20250930_2021_01_02_PROD.dta", clear

// estimate poverty for various poverty lines
forval l = 0.5(0.5)25 {
	local k = `l'*10
	bys code year: gen pov`k' = welf<`l'
}

collapse (mean) pov* (rawsum) pop [aw=pop], by(code year)

reshape long pov, i(code year pop) j(povertyline)
replace povertyline = povertyline/10

