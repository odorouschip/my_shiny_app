library(shiny)
library(jsonlite)

# Fourier helpers
resample_path <- function(x, y, n_points = 300) {
  dx <- diff(x); dy <- diff(y); ds <- sqrt(dx^2 + dy^2)
  s <- c(0, cumsum(ds)); total <- s[length(s)]
  if (total < 1e-6) return(list(x = x, y = y))
  ts <- seq(0, total, length.out = n_points + 1)[1:n_points]
  list(x = approx(s, x, xout = ts, rule = 2)$y,
       y = approx(s, y, xout = ts, rule = 2)$y)
}

compute_fourier <- function(px, py, max_terms = 150) {
  N <- length(px)
  z <- complex(real = px, imaginary = py)
  max_n <- min(max_terms, floor(N / 2))
  freqs <- 0L
  for (i in seq_len(max_n)) freqs <- c(freqs, i, -i)
  freqs <- freqs[seq_len(min(length(freqs), max_terms))]
  k_over_N <- (0:(N - 1)) / N
  coeffs <- vapply(freqs, function(n) {
    mean(z * exp(-2i * pi * n * k_over_N))
  }, complex(1))
  amps <- Mod(coeffs)
  ord <- order(amps, decreasing = TRUE)
  data.frame(
    freq = freqs[ord], re = Re(coeffs[ord]), im = Im(coeffs[ord]),
    amp = round(amps[ord], 4), phase = round(Arg(coeffs[ord]), 4)
  )
}

# Game metadata — ASCII-safe icons (emojis rendered via HTML entities)
games <- list(
  list(id = "monty_hall", title = "Monty Hall Problem", icon = "&#x1F6AA;",
       desc = "Should you switch doors? Play the famous probability puzzle.",
       color = "#e07a5f", tag = "Probability"),
  list(id = "fourier", title = "Fourier Drawing", icon = "&#x1F300;",
       desc = "Draw any shape and watch spinning circles recreate it.",
       color = "#3d85c6", tag = "Calculus"),
  list(id = "dating", title = "Dating Number", icon = "&#x1F498;",
       desc = "How many should you date before committing? Optimal stopping!",
       color = "#c27ba0", tag = "Optimization"),
  list(id = "plinko", title = "Plinko", icon = "&#x1F4CC;",
       desc = "Drop balls through pegs and watch the bell curve emerge.",
       color = "#6aa84f", tag = "Statistics"),
  list(id = "buffon", title = "Buffon's Needle", icon = "&#x1F4CF;",
       desc = "Toss needles on lined paper to estimate pi!",
       color = "#e69138", tag = "Geometry"),
  list(id = "poker", title = "Poker Hands", icon = "&#x1F0CF;",
       desc = "Deal hands and discover how rare each poker hand is.",
       color = "#674ea7", tag = "Combinatorics"),
  list(id = "blackjack", title = "Blackjack", icon = "&#x1F0A1;",
       desc = "Hit or stand? Play 21 and learn basic strategy.",
       color = "#cc0000", tag = "Probability"),
  list(id = "roulette", title = "Roulette", icon = "&#x1F3B0;",
       desc = "Spin the wheel and learn why the house always wins.",
       color = "#45818e", tag = "Statistics")
)

# ════════════════════════════════════════════════════════════════
# CSS (separate string to stay under limits)
# ════════════════════════════════════════════════════════════════
css_base <- '
@import url("https://fonts.googleapis.com/css2?family=DM+Sans:opsz,wght@9..40,300;9..40,400;9..40,500;9..40,700&family=DM+Serif+Display&family=JetBrains+Mono:wght@400;500&display=swap");
*{box-sizing:border-box;margin:0;padding:0}
:root{--bg:#faf7f2;--bg-card:#ffffff;--text:#2d2a26;--text-sub:#7a756d;--border:#e8e2d9;--accent:#d4643b;--radius:14px}
body{background:var(--bg);color:var(--text);font-family:"DM Sans",sans-serif;overflow-x:hidden}
.container-fluid{padding:0!important;background:transparent}
.hero{text-align:left;padding:60px 48px 18px;max-width:1200px}
.hero-title{font-family:"DM Serif Display",serif;font-size:5rem;letter-spacing:-1.5px;margin-bottom:12px}
.hero-sub{font-size:1.55rem;color:var(--text-sub);font-weight:300;max-width:700px;line-height:1.6}
.carousel-section{overflow:visible;padding:24px 0 80px;position:relative}
.carousel-section::before,.carousel-section::after{content:"";position:absolute;width:160px;z-index:2;pointer-events:none}
.carousel-section::before{left:0;top:0;bottom:0;background:linear-gradient(90deg,var(--bg),transparent)}
.carousel-section::after{right:0;top:-140px;bottom:-60px;background:linear-gradient(270deg,var(--bg),transparent)}
.carousel-track-wrapper{transform:rotate(-4deg);margin:0 -100px}
.carousel-track{display:flex;gap:28px;width:max-content;padding:18px 0;animation:carousel-scroll 40s linear infinite}
.carousel-track-offset{animation-delay:-20s;margin-left:164px}
.carousel-item{flex-shrink:0;width:300px;border-radius:18px;overflow:hidden;cursor:pointer;transition:transform .3s,box-shadow .3s;box-shadow:0 4px 24px rgba(0,0,0,.08);background:#fff}
.carousel-item:hover{transform:scale(1.07);box-shadow:0 12px 40px rgba(0,0,0,.18)}
.carousel-img{height:210px;display:flex;align-items:center;justify-content:center;font-size:5.5rem}
.carousel-label{background:#fff;padding:20px 22px 18px}
.carousel-label-tag{font-size:.78rem;text-transform:uppercase;letter-spacing:1.2px;font-weight:700;margin-bottom:5px}
.carousel-label-title{font-family:"DM Serif Display",serif;font-size:1.4rem;color:var(--text)}
.carousel-label-desc{font-size:.9rem;color:var(--text-sub);line-height:1.45;margin-top:5px}
@keyframes carousel-scroll{0%{transform:translateX(0)}100%{transform:translateX(-50%)}}
'

css_layout <- '
.game-topbar{display:flex;align-items:center;gap:14px;padding:12px 28px;border-bottom:1px solid var(--border);background:var(--bg-card)}
.back-btn{display:inline-flex;align-items:center;gap:6px;padding:7px 16px;border:1px solid var(--border);border-radius:8px;background:var(--bg);color:var(--text);font-family:"DM Sans",sans-serif;font-size:.85rem;font-weight:500;cursor:pointer;transition:all .15s}
.back-btn:hover{background:var(--border)}
.topbar-title{font-family:"DM Serif Display",serif;font-size:1.15rem}
.g-wrap{max-width:900px;margin:24px auto;padding:0 24px 40px}
.g-card{background:var(--bg-card);border:1px solid var(--border);border-radius:var(--radius);padding:20px;margin-bottom:16px}
.g-card h3{font-size:.78rem;color:var(--text-sub);text-transform:uppercase;letter-spacing:1.5px;margin-bottom:12px;font-weight:700}
.g-btn{padding:10px 22px;border:none;border-radius:10px;font-family:"DM Sans",sans-serif;font-weight:600;font-size:.88rem;cursor:pointer;transition:all .18s}
.g-btn:hover{transform:translateY(-1px);filter:brightness(1.08)}
.g-btn:active{transform:translateY(0)}
.g-btn:disabled{opacity:.35;cursor:not-allowed;transform:none!important;filter:none!important}
.g-btn-primary{background:var(--accent);color:#fff}
.g-btn-secondary{background:var(--bg);color:var(--text-sub);border:1px solid var(--border)}
.g-row{display:flex;gap:10px;flex-wrap:wrap;align-items:center}
.g-stat{font-family:"JetBrains Mono",monospace;font-size:.85rem;color:var(--text);padding:6px 14px;background:var(--bg);border-radius:8px;border:1px solid var(--border)}
.g-stat b{color:var(--accent)}
.g-info{background:color-mix(in srgb,var(--accent) 6%,transparent);border:1px solid color-mix(in srgb,var(--accent) 15%,transparent);border-radius:10px;padding:14px 16px;font-size:.85rem;line-height:1.5;margin-top:12px}
.g-canvas-wrap{border-radius:var(--radius);overflow:hidden;border:1px solid var(--border);background:#1a1a2e;position:relative}
.g-canvas-wrap canvas{display:block;width:100%;height:100%}
.g-msg{text-align:center;font-size:1.05rem;padding:14px;font-family:"DM Serif Display",serif;min-height:48px}
'

css_games <- '
.playing-card{display:inline-flex;flex-direction:column;align-items:center;justify-content:center;width:64px;height:90px;background:#fff;border:2px solid #ddd;border-radius:8px;font-size:1.05rem;font-weight:700;margin:3px;box-shadow:0 2px 6px rgba(0,0,0,.08);line-height:1.1}
.playing-card.red{color:#c0392b}.playing-card.black{color:#1a1a1a}
.playing-card .card-suit{font-size:1.3rem}
.card-row{display:flex;flex-wrap:wrap;gap:4px;justify-content:center;margin:10px 0}
.door-row{display:flex;gap:16px;justify-content:center;margin:16px 0}
.door{width:110px;height:160px;background:linear-gradient(180deg,#8B6914,#6B4F12);border:3px solid #5a4210;border-radius:6px 6px 0 0;cursor:pointer;display:flex;align-items:center;justify-content:center;transition:all .2s;position:relative;box-shadow:0 4px 12px rgba(0,0,0,.15)}
.door:hover:not(.opened):not(.disabled){transform:translateY(-4px);box-shadow:0 8px 20px rgba(0,0,0,.2)}
.door .door-num{position:absolute;top:8px;left:50%;transform:translateX(-50%);font-size:.8rem;font-weight:700;color:#d4a943;font-family:"DM Sans",sans-serif}
.door.opened{background:linear-gradient(180deg,#d4c5a0,#bfae8a);border-color:#a89870;cursor:default}
.door.disabled{cursor:default;opacity:.7}
.door.winner{border-color:#27ae60;box-shadow:0 0 20px rgba(39,174,96,.3)}
.door-content{font-size:2.8rem}
.canvas-badge{position:absolute;top:10px;left:10px;background:rgba(0,0,0,.55);color:#ccc;font-size:.75rem;padding:4px 12px;border-radius:20px;pointer-events:none;font-family:"DM Sans",sans-serif}
.f-btn-preset.active{border-color:var(--accent)!important;color:var(--accent)!important;background:color-mix(in srgb,var(--accent) 8%,transparent)!important}
.coeff-table{width:100%;border-collapse:collapse;font-family:"JetBrains Mono",monospace;font-size:.75rem}
.coeff-table th{color:var(--text-sub);font-weight:500;text-align:left;padding:4px 6px;border-bottom:1px solid var(--border)}
.coeff-table td{padding:3px 6px}
.roul-wheel-wrap{width:320px;height:320px;margin:0 auto;position:relative}
.roul-canvas{width:320px;height:320px}
.bet-grid{display:grid;grid-template-columns:repeat(6,1fr);gap:4px;margin:10px 0}
.bet-cell{padding:8px 4px;text-align:center;font-size:.75rem;font-weight:600;border:2px solid var(--border);border-radius:6px;cursor:pointer;transition:all .15s;font-family:"JetBrains Mono",monospace}
.bet-cell:hover{transform:scale(1.05)}
.bet-cell.red-num{background:#e74c3c;color:#fff;border-color:#c0392b}
.bet-cell.black-num{background:#2c3e50;color:#fff;border-color:#1a252f}
.bet-cell.green-num{background:#27ae60;color:#fff;border-color:#1e8449}
.bet-cell.selected{box-shadow:0 0 0 3px var(--accent);transform:scale(1.08)}
.bet-cell.outside{background:var(--bg);grid-column:span 2;font-size:.72rem}
@media(max-width:520px){.hero-title{font-size:1.8rem}.carousel-item{width:220px}.carousel-img{height:160px;font-size:3.5rem}.door{width:85px;height:120px}}
'

# ════════════════════════════════════════════════════════════════
# GAME UI FUNCTIONS
# ════════════════════════════════════════════════════════════════
monty_ui <- function() {
  div(id = "monty-game", class = "g-wrap",
      div(class = "g-card", h3("Pick a Door"),
          div(class = "g-msg", id = "mh-msg", "Pick a door!"),
          div(class = "door-row",
              div(class = "door", div(class = "door-num", "1"), div(class = "door-content", "?")),
              div(class = "door", div(class = "door-num", "2"), div(class = "door-content", "?")),
              div(class = "door", div(class = "door-num", "3"), div(class = "door-content", "?"))
          ),
          div(class = "g-row", style = "justify-content:center;margin-top:12px",
              tags$button(id = "mh-switch", class = "g-btn g-btn-primary",
                          onclick = "mhSwitch()", style = "display:none", "Switch!"),
              tags$button(id = "mh-stay", class = "g-btn g-btn-secondary",
                          onclick = "mhStay()", style = "display:none", "Stay"),
              tags$button(class = "g-btn g-btn-secondary", onclick = "mhNew()", "New Round")
          )
      ),
      div(class = "g-card", h3("Statistics"),
          div(id = "mh-stats", class = "g-stat",
              HTML("<b>Switch:</b> 0/0 (--%) &nbsp; <b>Stay:</b> 0/0 (--%)")),
          div(class = "g-info",
              HTML("<b>The math:</b> When you switch, you win 2/3 of the time! The host always reveals a goat, so switching effectively gives you two doors instead of one."))
      )
  )
}

fourier_ui <- function() {
  div(id = "fourier-game",
      div(style = "display:flex;gap:18px;max-width:1100px;margin:24px auto;padding:0 24px 36px;flex-wrap:wrap;justify-content:center",
          div(class = "g-canvas-wrap", style = "width:520px;height:520px;flex-shrink:0",
              tags$canvas(id = "mainCanvas"),
              div(class = "canvas-badge", "Draw something!")
          ),
          div(style = "width:310px;display:flex;flex-direction:column;gap:14px",
              div(class = "g-card", h3("Controls"), div(class = "g-row",
                                                        tags$button(id = "animateBtn", class = "g-btn g-btn-primary",
                                                                    onclick = "startAnimation()", disabled = NA, "Animate!"),
                                                        tags$button(class = "g-btn g-btn-secondary", onclick = "clearCanvas()", "Clear")
              )),
              div(class = "g-card", h3("Try a shape"), div(class = "g-row",
                                                           tags$button(class = "g-btn g-btn-secondary f-btn-preset",
                                                                       onclick = "loadPreset('circle')", "Circle"),
                                                           tags$button(class = "g-btn g-btn-secondary f-btn-preset",
                                                                       onclick = "loadPreset('star')", "Star"),
                                                           tags$button(class = "g-btn g-btn-secondary f-btn-preset",
                                                                       onclick = "loadPreset('heart')", "Heart"),
                                                           tags$button(class = "g-btn g-btn-secondary f-btn-preset",
                                                                       onclick = "loadPreset('figure8')", "Figure 8")
              )),
              div(class = "g-card", h3("Settings"),
                  div(style = "margin-bottom:8px",
                      div(style = "display:flex;justify-content:space-between;font-size:.82rem;color:var(--text-sub);margin-bottom:4px",
                          tags$span("Fourier terms"),
                          tags$span(id = "termsValue",
                                    style = "font-family:JetBrains Mono,monospace;color:var(--accent)", "50")),
                      tags$input(type = "range", min = 1, max = 150, value = 50,
                                 oninput = "updateTerms(this.value)")
                  ),
                  div(
                    div(style = "display:flex;justify-content:space-between;font-size:.82rem;color:var(--text-sub);margin-bottom:4px",
                        tags$span("Speed"),
                        tags$span(id = "speedValue",
                                  style = "font-family:JetBrains Mono,monospace;color:var(--accent)", "1x")),
                    tags$input(type = "range", min = 0.2, max = 3, value = 1, step = 0.1,
                               oninput = "updateSpeed(this.value)")
                  )
              ),
              div(class = "g-card", h3("The Math"),
                  div(style = "font-family:JetBrains Mono,monospace;font-size:.82rem;color:var(--accent);background:color-mix(in srgb,var(--accent) 6%,transparent);padding:10px;border-radius:8px;text-align:center;line-height:1.5;margin-bottom:10px",
                      HTML("f(t) = &Sigma; c<sub>n</sub> &middot; e<sup>i&middot;n&middot;t</sup>"), br(),
                      tags$span(style = "font-size:.72rem;color:var(--text-sub)",
                                "Each term = one spinning circle!")),
                  uiOutput("coeff_table")
              )
          )
      )
  )
}

dating_ui <- function() {
  div(id = "dating-game", class = "g-wrap",
      div(class = "g-card", h3("Setup"), div(class = "g-row",
                                             tags$label(style = "font-size:.85rem;color:var(--text-sub)", "Pool size:"),
                                             tags$input(id = "dat-n", type = "number", min = 5, max = 100, value = 20,
                                                        style = "width:70px;padding:6px 10px;border:1px solid var(--border);border-radius:8px;font-family:DM Sans,sans-serif;font-size:.9rem"),
                                             tags$button(class = "g-btn g-btn-primary", onclick = "datStart()", "Start Game")
      )),
      div(class = "g-card",
          h3(HTML("Candidate &mdash; Pool of <span id='dat-n-show'>20</span>")),
          div(class = "g-msg", id = "dat-msg", "Set your pool size and start!"),
          div(id = "dat-card-area", style = "min-height:120px"),
          div(class = "g-row", style = "justify-content:center;margin-top:12px",
              tags$button(id = "dat-accept", class = "g-btn g-btn-primary",
                          onclick = "datAccept()", disabled = NA, "Accept"),
              tags$button(id = "dat-reject", class = "g-btn g-btn-secondary",
                          onclick = "datReject()", disabled = NA, "Reject")
          ),
          div(id = "dat-result", style = "margin-top:14px")
      )
  )
}

plinko_ui <- function() {
  div(id = "plinko-game", class = "g-wrap",
      div(class = "g-card", h3("Controls"), div(class = "g-row",
                                                tags$button(class = "g-btn g-btn-primary", onclick = "plinkoDrop(1)", "Drop 1"),
                                                tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoDrop(10)", "Drop 10"),
                                                tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoDrop(100)", "Drop 100"),
                                                tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoReset()", "Reset"),
                                                div(class = "g-stat", "Balls: ", tags$b(id = "plinko-count", "0"))
      )),
      div(class = "g-card", style = "padding:10px",
          div(class = "g-canvas-wrap",
              style = "width:600px;height:500px;max-width:100%;margin:0 auto",
              tags$canvas(id = "plinko-canvas"))
      ),
      div(class = "g-info",
          HTML("<b>The math:</b> Each ball makes independent left/right choices at each peg. By the Central Limit Theorem, the sum follows a normal (bell curve) distribution. More balls = clearer bell shape!"))
  )
}

buffon_ui <- function() {
  div(id = "buffon-game", class = "g-wrap",
      div(class = "g-card", h3("Drop Needles"), div(class = "g-row",
                                                    tags$button(class = "g-btn g-btn-primary", onclick = "bufDrop(1)", "Drop 1"),
                                                    tags$button(class = "g-btn g-btn-secondary", onclick = "bufDrop(10)", "Drop 10"),
                                                    tags$button(class = "g-btn g-btn-secondary", onclick = "bufDrop(100)", "Drop 100"),
                                                    tags$button(class = "g-btn g-btn-secondary", onclick = "bufReset()", "Reset")
      )),
      div(class = "g-card", style = "padding:10px",
          div(class = "g-canvas-wrap",
              style = "width:600px;height:400px;max-width:100%;margin:0 auto",
              tags$canvas(id = "buffon-canvas"))
      ),
      div(class = "g-card", h3("Results"),
          div(class = "g-row", style = "flex-wrap:wrap;gap:10px",
              div(class = "g-stat", "Needles: ", tags$b(id = "buf-total", "0")),
              div(class = "g-stat", "Crossings: ", tags$b(id = "buf-hits", "0")),
              div(class = "g-stat", HTML("&pi; estimate: "), tags$b(id = "buf-pi", "--")),
              div(class = "g-stat", "Error: ", tags$b(id = "buf-err", "--"))
          ),
          div(class = "g-info",
              HTML("<b>The math:</b> P(crossing) = 2L/(&pi;D), so &pi; = 2L&middot;N/(D&middot;H). The more needles you drop, the closer to &pi; &asymp; 3.14159!"))
      )
  )
}

poker_ui <- function() {
  div(id = "poker-game", class = "g-wrap",
      div(class = "g-card", h3("Your Hand"),
          div(class = "g-row", style = "justify-content:center;margin-bottom:10px",
              tags$button(class = "g-btn g-btn-primary", onclick = "pokerDeal()", "Deal 5 Cards"),
              tags$button(class = "g-btn g-btn-secondary", onclick = "pokerDeal1000()", "Deal 1000"),
              tags$button(class = "g-btn g-btn-secondary", onclick = "pokerReset()", "Reset Stats")
          ),
          div(id = "poker-cards", class = "card-row"),
          div(id = "poker-result", class = "g-msg")
      ),
      div(class = "g-card", h3("Hand Frequencies"), div(id = "poker-stats"))
  )
}

blackjack_ui <- function() {
  div(id = "blackjack-game", class = "g-wrap",
      div(class = "g-card", h3("Table"),
          div(class = "g-row", style = "margin-bottom:10px",
              div(class = "g-stat", "Chips: $", tags$b(id = "bj-chips", "100")),
              tags$label(style = "font-size:.82rem;color:var(--text-sub)", "Bet: $"),
              tags$input(id = "bj-bet-input", type = "number", min = 1, max = 100, value = 10,
                         style = "width:60px;padding:5px 8px;border:1px solid var(--border);border-radius:8px;font-family:DM Sans,sans-serif")
          ),
          div(id = "bj-dealer", style = "min-height:80px"),
          tags$hr(style = "border:none;border-top:1px dashed var(--border);margin:10px 0"),
          div(id = "bj-player", style = "min-height:80px"),
          div(class = "g-msg", id = "bj-msg", "Place your bet and deal!"),
          div(class = "g-row", style = "justify-content:center;margin-top:10px",
              tags$button(id = "bj-deal", class = "g-btn g-btn-primary", onclick = "bjDeal()", "Deal"),
              tags$button(id = "bj-hit", class = "g-btn g-btn-secondary",
                          onclick = "bjHit()", disabled = NA, "Hit"),
              tags$button(id = "bj-stand", class = "g-btn g-btn-secondary",
                          onclick = "bjStand()", disabled = NA, "Stand"),
              tags$button(class = "g-btn g-btn-secondary", onclick = "bjReset()", "Reset")
          )
      ),
      div(class = "g-info",
          HTML("<b>The math:</b> Blackjack has one of the lowest house edges (~0.5% with basic strategy). Your decision to hit or stand depends on the dealer&rsquo;s visible card and your probability of busting."))
  )
}

roulette_ui <- function() {
  reds <- c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
  num_cells <- lapply(0:36, function(n) {
    cls <- if (n == 0) "bet-cell green-num"
    else if (n %in% reds) "bet-cell red-num"
    else "bet-cell black-num"
    tags$div(class = cls,
             onclick = paste0("roulBet('number','", n, "',this)"),
             as.character(n))
  })
  outside <- tagList(
    tags$div(class = "bet-cell outside", onclick = "roulBet('color','red',this)", "Red"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('color','black',this)", "Black"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('parity','even',this)", "Even"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('parity','odd',this)", "Odd"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('range','1-18',this)", "1-18"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('range','19-36',this)", "19-36")
  )
  
  div(id = "roulette-game", class = "g-wrap",
      div(class = "g-card", style = "text-align:center", h3("Wheel"),
          div(class = "roul-wheel-wrap",
              tags$canvas(id = "roul-canvas", class = "roul-canvas")),
          div(id = "roul-result", style = "margin:10px 0;min-height:36px"),
          div(class = "g-msg", id = "roul-msg", "Place a bet!")
      ),
      div(class = "g-card", h3("Place Your Bet"),
          div(class = "g-row", style = "margin-bottom:10px",
              div(class = "g-stat", "Chips: $", tags$b(id = "roul-chips", "100")),
              tags$label(style = "font-size:.82rem;color:var(--text-sub)", "Bet: $"),
              tags$input(id = "roul-bet-amt", type = "number", min = 1, max = 100, value = 10,
                         style = "width:60px;padding:5px 8px;border:1px solid var(--border);border-radius:8px;font-family:DM Sans,sans-serif")
          ),
          div(class = "bet-grid", num_cells, outside),
          div(class = "g-row", style = "justify-content:center;margin-top:12px",
              tags$button(id = "roul-spin", class = "g-btn g-btn-primary",
                          onclick = "roulSpin()", "Spin!"),
              tags$button(class = "g-btn g-btn-secondary", onclick = "roulReset()", "Reset")
          )
      ),
      div(class = "g-info",
          HTML("<b>The math:</b> European roulette has 37 slots (0&ndash;36). A number bet pays 35:1 but true odds are 36:1 &mdash; that 2.7% gap is the house edge. Over many spins, the house always wins!"))
  )
}

# ════════════════════════════════════════════════════════════════
# SHINY APP
# ════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$style(HTML(css_base)),
    tags$style(HTML(css_layout)),
    tags$style(HTML(css_games)),
    tags$meta(name = "viewport", content = "width=device-width,initial-scale=1"),
    tags$script(src = "games.js")
  ),
  uiOutput("main_view")
)

server <- function(input, output, session) {
  current_page <- reactiveVal("home")
  observeEvent(input$nav_target, { current_page(input$nav_target) })
  
  output$main_view <- renderUI({
    page <- current_page()
    if (page == "home") {
      make_items <- function(game_list) {
        lapply(game_list, function(g) {
          div(class = "carousel-item",
              onclick = paste0("goToGame('", g$id, "')"),
              div(class = "carousel-img",
                  style = paste0("background:", g$color, ";"),
                  HTML(g$icon)),
              div(class = "carousel-label",
                  div(class = "carousel-label-tag",
                      style = paste0("color:", g$color, ";"), g$tag),
                  div(class = "carousel-label-title", g$title),
                  div(class = "carousel-label-desc", g$desc))
          )
        })
      }
      all_items <- make_items(games)
      return(tagList(
        div(class = "hero",
            div(class = "hero-title", "APMA Math Arcade"),
            div(class = "hero-sub",
                "Interactive math and statistics games \u2014 pick one and play!")
        ),
        div(class = "carousel-section",
            div(class = "carousel-track-wrapper",
                div(class = "carousel-track", all_items, all_items),
                div(class = "carousel-track carousel-track-offset", all_items, all_items)))
      ))
    }
    
    # Game page
    g <- Filter(function(x) x$id == page, games)
    if (length(g) == 0) return(NULL)
    g <- g[[1]]
    topbar <- div(class = "game-topbar",
                  tags$button(class = "back-btn", onclick = "goHome()",
                              HTML("&larr; Back")),
                  div(class = "topbar-title", HTML(paste(g$icon, g$title)))
    )
    content <- switch(page,
                      monty_hall = monty_ui(),
                      fourier    = fourier_ui(),
                      dating     = dating_ui(),
                      plinko     = plinko_ui(),
                      buffon     = buffon_ui(),
                      poker      = poker_ui(),
                      blackjack  = blackjack_ui(),
                      roulette   = roulette_ui()
    )
    div(class = "game-page-wrapper", topbar, content)
  })
  
  # Fourier server logic
  fourier_data <- reactiveVal(NULL)
  
  observeEvent(input$drawn_points, {
    pts <- fromJSON(input$drawn_points)
    req(nrow(pts) > 10)
    gap <- sqrt((pts$x[nrow(pts)] - pts$x[1])^2 +
                  (pts$y[nrow(pts)] - pts$y[1])^2)
    if (gap > 2) {
      nc <- max(30, round(nrow(pts) * 0.15))
      cx <- seq(pts$x[nrow(pts)], pts$x[1], length.out = nc + 1)[-1]
      cy <- seq(pts$y[nrow(pts)], pts$y[1], length.out = nc + 1)[-1]
      pts <- rbind(pts, data.frame(x = cx, y = cy))
    }
    rs <- resample_path(pts$x, pts$y, n_points = 300)
    fc <- compute_fourier(rs$x, rs$y, max_terms = 150)
    fourier_data(fc)
    cl <- lapply(seq_len(nrow(fc)), function(i) {
      list(freq = fc$freq[i], amp = fc$amp[i], phase = fc$phase[i])
    })
    session$sendCustomMessage("fourier_coeffs", cl)
  })
  
  observeEvent(input$clear_signal, { fourier_data(NULL) })
  
  output$coeff_table <- renderUI({
    fc <- fourier_data()
    if (is.null(fc)) {
      return(div(style = "font-size:.82rem;color:var(--text-sub);text-align:center;padding:8px",
                 "Draw a shape to see coefficients"))
    }
    tn <- min(8, nrow(fc))
    rows <- lapply(seq_len(tn), function(i) {
      tags$tr(tags$td(fc$freq[i]), tags$td(fc$amp[i]), tags$td(fc$phase[i]))
    })
    extra <- if (nrow(fc) > tn) {
      div(style = "color:var(--text-sub);font-size:.72rem;text-align:center;margin-top:4px",
          paste0("+ ", nrow(fc) - tn, " more"))
    }
    tagList(
      tags$table(class = "coeff-table",
                 tags$thead(tags$tr(
                   tags$th("Freq"), tags$th("Radius"), tags$th("Phase")
                 )),
                 tags$tbody(rows)
      ),
      extra
    )
  })
}

shinyApp(ui = ui, server = server)