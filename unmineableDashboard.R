library("shiny")
library("httr")
library("dygraphs")
library("dplyr")
library("jsonlite")
library("lubridate")
library("xts")

ui <- fluidPage(

    # Application title
    titlePanel("Unmineable Dashboard"),

    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "address",
                        "Enter Your Address:",
                        value  = "0x9fdf89f6e0ab5b7b3e982035ed871da6c7b78890"),
            selectInput(inputId = "coin",
                        label = "Select Your Coin:",
                        choices = c("SHIB","1INCH", "CAKE",
                                    "CHZ","HOT", "MATIC",
                                    "SOL", "AAVE", "ADA",
                                    "ALGO", "ATOM", "BAND",
                                    "BAT", "BCH", "BNB",
                                    "BTC", "BTG", "BTT",
                                    "DASH", "DGB", "DOGE",
                                    "ENJ", "EOS", "ETC",
                                    "ETH", "FUN", "GAS",
                                    "ICX", "KLV", "KNC",
                                    "LINK", "LSK", "LTC",
                                    "MANA", "MTL", "NANO",
                                    "NEO", "QTUM", "REP",
                                    "RSR", "RVN", "SC",
                                    "SKY", "SUSHI", "TRX",
                                    "UNI", "USDT", "VET",
                                    "WAVES", "WBTC", "WIN",
                                    "XLM", "XMR", "XRP",
                                    "XTZ", "XVG", "YFI",
                                    "ZEC", "ZIL", "ZRX")
                        ),
            radioButtons(inputId = "convert_to",
                        label = "Convert to:",
                        choices = c("USD", "BTC", "ETH"))
        ),

        mainPanel(
            h3(verbatimTextOutput("balance")),
            dygraphOutput("hashrate")
        )
    )
)

server <- function(input, output) {

    output$balance <- renderText({
        address = input$address
        coin = input$coin
        # UNMINEABLE API CALLS -- Get data into a dataframe
        unmineable_base_url <- "https://api.unminable.com/v4/"
        unmineable_action <- "address/"
        response <- httr::GET(url = paste0(unmineable_base_url, unmineable_action,address, "?", "coin=",  coin))
        content <- httr::content(response, as= "text")
        content <- jsonlite::fromJSON(content)$data[1:12] %>% data.frame()
        # END UNMINEABLE API CALLS -- Get data into a dataframe

        # GET USD conversions
        coinbase_base_url <- "https://api.coinbase.com/v2/"
        coinbase_action <- "prices/"
        currency_pair <- paste0(coin, "-USD")
        conversion_response <- httr::GET(url = paste0(coinbase_base_url, coinbase_action, currency_pair, "/spot"))
        conversion_content <- httr::content(conversion_response, as= "text")
        conversion_content <- jsonlite::fromJSON(conversion_content)$data[3] %>% data.frame()
        options(scipen = 99999)
        usd_bal <- as.numeric(paste0(conversion_content$amount)) * as.numeric(paste0(content$balance))
        # END USD conversions

        # BTC Conversion
        btc_conversion <- httr::GET(url = paste0(coinbase_base_url, coinbase_action, "BTC-USD", "/spot"))
        btc_conversion <- httr::content(btc_conversion, as= "text")
        btc_conversion <- jsonlite::fromJSON(btc_conversion)$data[3] %>% data.frame()
        btc_amount <- usd_bal/  as.numeric(paste0(btc_conversion$amount))
        # END BTC Conversion

        # ETH Conversion
        eth_conversion <- httr::GET(url = paste0(coinbase_base_url, coinbase_action, "ETH-USD", "/spot"))
        eth_conversion <- httr::content(eth_conversion, as= "text")
        eth_conversion <- jsonlite::fromJSON(eth_conversion)$data[3] %>% data.frame()
        eth_amount <- usd_bal/  as.numeric(paste0(eth_conversion$amount))
        # END ETH Conversion

        # END GET USD current conversions
        # RETURN BALANCES
        return(paste("Current ", input$convert_to, " value is ",
                     switch(input$convert_to, BTC = btc_amount, USD = usd_bal, ETH = eth_amount),
                     "\n",
                    "Current SHIBA: ", content$balance))
        # END RETURN BALANCES

    })
    output$hashrate <- renderDygraph({
        address <- input$address
        coin <- input$coin
        # UNMINEABLE API CALLS -- Get data into a dataframe
        unmineable_base_url <- "https://api.unminable.com/v4/"
        unmineable_action <- "address/"
        response <- httr::GET(url = paste0(unmineable_base_url, unmineable_action,address, "?", "coin=",  coin))
        content <- httr::content(response, as= "text")
        content <- jsonlite::fromJSON(content)$data[1:12] %>% data.frame()

        # HISTORICAL HASHRATE GRAPH
        uuid <- content$uuid
        hashrate_action <- "account/"
        end_url_str <- "/workers"
        graph_unmineable <- httr::GET(url = paste0(unmineable_base_url, hashrate_action,uuid,  end_url_str))
        graph_unmineable_content <- httr::content(graph_unmineable, type = "application/json")
        graph <- data.frame(hashrate = graph_unmineable_content$data$ethash$chart$reported$data %>% unlist(),
                            time = graph_unmineable_content$data$ethash$chart$reported$timestamps %>% unlist())
        graph <- graph %>% filter(row_number() %% 5 == 1)
        graph <- graph %>% mutate(date_time = format(as.POSIXct(time / 1000, origin = "1970-01-01", tz =Sys.timezone()), "%Y-%m-%d %H:%M:%S"))
        graph <- graph %>% mutate(date_time = lubridate::ymd_hms(date_time))
        graph <- tail(graph, 100)
        graph <- xts(graph$hashrate, graph$date_time)
        # END HISTORICAL HASHRATE

        # Plot Hashrate
        dygraph(graph, main = "Unmineable Hashrate", xlab = "Time", ylab = "Hashrate (Mhs)") %>% dyOptions(fillGraph = TRUE,
                                                                   fillAlpha = .4)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
