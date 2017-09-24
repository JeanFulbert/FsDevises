namespace FsDevises

type Rate = decimal

type Currency = Currency of string 

type ExchangeRate = {
    source: Currency
    destination: Currency
    rate: Rate
}


type RateByCurrencies = Map<Currency, Map<Currency, Rate>>