
<!-- README.md is generated from README.Rmd. Please edit that file -->

# palver - The Palver Data API package for R!

<!-- badges: start -->
<!-- badges: end -->

The Palver Data API package provide functions for simple usage of our
data by clients in R. [Palver](https://www.palver.com.br/) is a startup
for social listening, we collect and analyze data from a diverse range
of sources, such as, public WhatsApp chats, public Telegram chats,
TikTok profiles, Reddit forums, radio stations and much more.

## Installation

You can install the development version of palver:

``` r
install.packages('devtools')

devtools::install_github(repo = 'palverdata/palver_r_package')
```

## Example

Here we provide examples on how to use palver functions in order to
request data from Palver API. Firstly, you should have a login at our
plataform that enable you to collect Palver data, if you want to
understand how our API works, send an e-mail to <contato@palver.com.br>.

Now, if you have a login, your e-mail and password for the login enable
you to have a token, which is a key to access our data.

Of course, the example below should return an error, as we can not
provide a real login, but you can have an idea of what your command
should look like!

``` r
library(palver)

# The code below will return an error: of course, imagine if a fisherman would have such obvious e-mail and password!

# my_token <- get_token(email = 'fisherman@sea.com', password = 'ilovefish')
```

With your token in hands, we can show how you can make requests in our
data set that returns a tibble! Let’s say you want to have access to all
messages that contain the terms “fish” or “sea” or “boat”, but you do
not want messages with the word “mermaid”. Therefore, you can make this
[Boolean
Lucene](https://solr.apache.org/guide/6_6/the-standard-query-parser.html)
query: *(fish OR sea OR boat) AND NOT mermaid*. And you also want to see
it in messages from Telegram, only in English, and from first of January
of 2021 to second of March in 2023. The request should be as it follows:

``` r
# The code below will not work if you do not have a token

# request_messages(query = '(fish OR sea OR boat) AND NOT mermaid', source = 'telegram', lang = 'en', startDate = '2021-01-01T00:00:00.000Z', endDate = '2023-03-02T23:59:59.000Z', token = my_token)
```

Other request functions works as the same: you can request for chat
data, trends and messages.
