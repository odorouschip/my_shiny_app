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

# Game metadata — icon = topbar emoji (HTML entities); thumb = carousel image under www/
games <- list(
  list(id = "monty_hall", title = "Monty Hall Problem", icon = "&#x1F6AA;",
       thumb = "img/games/monty_hall.png",
       desc = "Should you switch doors? Play the famous probability puzzle.",
       color = "#e07a5f", tag = "Probability"),
  list(id = "fourier", title = "Fourier Drawing", icon = "&#x1F300;",
       thumb = "img/games/fourier.png",
       desc = "Draw any shape and watch spinning circles recreate it.",
       color = "#3d85c6", tag = "Calculus"),
  list(id = "dating", title = "Dating Number", icon = "&#x1F498;",
       thumb = "img/games/dating.png",
       desc = "How many should you date before committing? Optimal stopping!",
       color = "#c27ba0", tag = "Optimization"),
  list(id = "plinko", title = "Plinko", icon = "&#x1F4CC;",
       thumb = "img/games/plinko.png",
       desc = "Drop balls through pegs and watch the bell curve emerge.",
       color = "#6aa84f", tag = "Statistics"),
  list(id = "buffon", title = "Buffon's Needle", icon = "&#x1F4CF;",
       thumb = "img/games/buffon.png",
       desc = "Toss needles on lined paper to estimate pi!",
       color = "#e69138", tag = "Geometry"),
  list(id = "poker", title = "Poker Hands", icon = "&#x1F0CF;",
       thumb = "img/games/poker.png",
       desc = "Deal hands and discover how rare each poker hand is.",
       color = "#674ea7", tag = "Combinatorics"),
  list(id = "blackjack", title = "Blackjack", icon = "&#x1F0A1;",
       thumb = "img/games/blackjack.png",
       desc = "Hit or stand? Play 21 and learn basic strategy.",
       color = "#cc0000", tag = "Probability"),
  list(id = "roulette", title = "Roulette", icon = "&#x1F3B0;",
       thumb = "img/games/roulette.png",
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
.carousel-img{height:210px;position:relative;overflow:hidden;background:var(--border)}
.carousel-img::after{content:"";position:absolute;inset:0;background:linear-gradient(180deg,transparent 55%,color-mix(in srgb,var(--img-tint) 35%,transparent));pointer-events:none;z-index:1}
.carousel-thumb{width:100%;height:100%;object-fit:cover;display:block;position:relative;z-index:0}
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
.g-canvas-wrap-light{background:#f3ebe3}
.g-canvas-wrap-light .canvas-badge{background:rgba(255,250,246,.96);color:var(--text);border:1px solid var(--border);box-shadow:0 2px 10px rgba(45,42,38,.07)}
.g-canvas-wrap canvas{display:block;width:100%;height:100%}
.g-msg{text-align:center;font-size:1.05rem;padding:14px;font-family:"DM Serif Display",serif;min-height:48px}
'

css_games <- '
.playing-card{display:inline-flex;flex-direction:column;align-items:center;justify-content:center;width:64px;height:90px;background:#fff;border:2px solid #ddd;border-radius:8px;font-size:1.05rem;font-weight:700;margin:3px;box-shadow:0 2px 6px rgba(0,0,0,.08);line-height:1.1}
.playing-card.red{color:#c0392b}.playing-card.black{color:#1a1a1a}
.playing-card .card-suit{font-size:1.3rem}
#poker-game{max-width:1080px}
#poker-game .g-card{padding:26px 28px 28px}
#poker-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:18px}
#poker-game .g-btn{font-size:1.2rem;padding:15px 32px;border-radius:12px}
#poker-game .g-row{gap:14px}
#poker-game .card-row{gap:12px;margin:18px 0}
#poker-game .playing-card{width:96px;height:134px;font-size:1.42rem;border-radius:12px;border-width:3px;margin:5px;box-shadow:0 4px 16px rgba(0,0,0,.1)}
#poker-game .playing-card .card-suit{font-size:1.85rem}
#poker-game .g-msg{font-size:1.75rem;padding:22px 16px;min-height:68px}
#poker-game .poker-stats-table{width:100%;font-size:1.45rem;border-collapse:collapse;line-height:1.35}
#poker-game .poker-stats-table th{font-weight:700;padding:14px 14px}
#poker-game .poker-stats-table td{padding:11px 14px}
#poker-game .poker-stats-total{text-align:center;margin-top:14px;font-size:1.2rem;color:var(--text-sub)}
#dating-game{max-width:1080px}
#dating-game .g-card{padding:26px 28px 28px}
#dating-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:18px}
#dating-game .g-btn{font-size:1.2rem;padding:15px 32px;border-radius:12px}
#dating-game .g-row{gap:14px;align-items:center}
#dating-game .g-msg{font-size:1.75rem;padding:22px 16px;min-height:68px}
#dating-game .dating-pool-label{font-size:1.15rem;color:var(--text-sub)}
#dating-game .dating-pool-input{width:96px;padding:10px 16px;border:1px solid var(--border);border-radius:10px;font-family:DM Sans,sans-serif;font-size:1.15rem}
#dating-game #dat-card-area{min-height:180px;display:flex;align-items:center;justify-content:center}
#dating-game .dating-cand-wrap{text-align:center}
#dating-game .dating-cand-icon{font-size:3.75rem;line-height:1}
#dating-game .dating-cand-sub{font-family:DM Serif Display,serif;font-size:1.9rem;margin-top:10px}
#dating-game .dating-cand-score{font-size:2.75rem;font-weight:700;color:var(--accent);margin:14px 0}
#dating-game .dating-cand-best{font-size:1.12rem;color:var(--text-sub)}
#dating-game .dating-actions{justify-content:center;margin-top:20px}
#dating-game #dat-result{margin-top:18px}
#dating-game .dating-result-msg{font-size:1.45rem;font-weight:600;line-height:1.45}
#dating-game .dating-result-fail{color:#c0392b}
#dating-game .dating-result-win{color:#27ae60}
#dating-game .dating-result-mid{color:#e67e22}
#dating-game #dat-result .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55;margin-top:14px}
#blackjack-game{max-width:1080px}
#blackjack-game .g-card{padding:26px 28px 28px}
#blackjack-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:18px}
#blackjack-game .g-btn{font-size:1.2rem;padding:15px 32px;border-radius:12px}
#blackjack-game .g-row{gap:14px;align-items:center}
#blackjack-game .g-stat{font-size:1.08rem;padding:8px 18px}
#blackjack-game .g-msg{font-size:1.75rem;padding:22px 16px;min-height:68px}
#blackjack-game .bj-toolbar{margin-bottom:14px}
#blackjack-game .bj-bet-label{font-size:1.15rem;color:var(--text-sub)}
#blackjack-game .bj-bet-input{width:96px;padding:10px 16px;border:1px solid var(--border);border-radius:10px;font-family:DM Sans,sans-serif;font-size:1.15rem}
#blackjack-game #bj-dealer,#blackjack-game #bj-player{min-height:100px}
#blackjack-game .bj-hand-label{font-size:1.08rem;color:var(--text-sub);margin-bottom:10px;font-weight:600}
#blackjack-game .card-row{gap:12px;margin:10px 0}
#blackjack-game .playing-card{width:96px;height:134px;font-size:1.42rem;border-radius:12px;border-width:3px;margin:5px;box-shadow:0 4px 16px rgba(0,0,0,.1)}
#blackjack-game .playing-card .card-suit{font-size:1.85rem}
#blackjack-game .bj-actions{justify-content:center;margin-top:20px}
#blackjack-game .bj-hr{border:none;border-top:1px dashed var(--border);margin:16px 0}
#blackjack-game .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55;margin-top:4px}
#monty-game{max-width:1080px}
#monty-game .g-card{padding:26px 28px 28px}
#monty-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:18px}
#monty-game .g-btn{font-size:1.2rem;padding:15px 32px;border-radius:12px}
#monty-game .g-row{gap:14px;align-items:center}
#monty-game .g-msg{font-size:1.75rem;padding:22px 16px;min-height:68px}
#monty-game .g-msg .mh-win{color:#27ae60;font-weight:700}
#monty-game .g-msg .mh-lose{color:#c0392b;font-weight:700}
#monty-game .g-stat{font-size:1.08rem;padding:12px 20px;line-height:1.5;display:block}
#monty-game .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55;margin-top:12px}
#monty-game .door-row{gap:32px;margin:24px 0}
#monty-game .door{width:150px;height:215px;border-width:4px;border-radius:8px 8px 0 0;box-shadow:0 6px 18px rgba(0,0,0,.12)}
#monty-game .door .door-num{font-size:1.05rem;top:12px}
#monty-game .door-content{font-size:3.5rem}
#monty-game .mh-actions{justify-content:center;margin-top:22px}
#fourier-game .fourier-layout{display:flex;gap:26px;max-width:1240px;margin:24px auto;padding:0 28px 44px;flex-wrap:wrap;justify-content:center;align-items:flex-start}
#fourier-game .fourier-canvas-area{width:680px;max-width:min(680px,calc(100vw - 56px));aspect-ratio:1;height:auto;flex-shrink:0}
#fourier-game .fourier-sidebar{width:340px;min-width:280px;flex:1 1 300px;display:flex;flex-direction:column;gap:16px}
#fourier-game .g-card{padding:22px 24px}
#fourier-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:14px}
#fourier-game .g-btn{font-size:1.2rem;padding:14px 22px;border-radius:12px}
#fourier-game .g-row{gap:12px;flex-wrap:wrap}
#fourier-game .fourier-slider-head{display:flex;justify-content:space-between;align-items:center;font-size:1.05rem;color:var(--text-sub);margin-bottom:8px}
#fourier-game .fourier-slider-head span[id]{font-family:JetBrains Mono,monospace;color:var(--accent)}
#fourier-game .fourier-formula{font-family:JetBrains Mono,monospace;font-size:1rem;color:var(--accent);background:color-mix(in srgb,var(--accent) 6%,transparent);padding:14px 14px;border-radius:10px;text-align:center;line-height:1.55;margin-bottom:12px}
#fourier-game .fourier-formula .fourier-formula-note{font-size:.92rem;color:var(--text-sub);display:block;margin-top:6px}
#fourier-game .coeff-table{font-size:.95rem}
#fourier-game .coeff-table th{padding:8px 8px}
#fourier-game .coeff-table td{padding:6px 8px}
#fourier-game .fourier-coeff-placeholder{font-size:1rem;color:var(--text-sub);text-align:center;padding:12px}
#fourier-game .fourier-coeff-more{color:var(--text-sub);font-size:.9rem;text-align:center;margin-top:6px}
#fourier-game .g-canvas-wrap-light .canvas-badge{font-size:.88rem;padding:6px 14px}
#fourier-game input[type=range]{width:100%;height:10px}
#buffon-game .buffon-layout{display:flex;gap:26px;max-width:1240px;margin:24px auto;padding:0 28px 44px;flex-wrap:wrap;justify-content:center;align-items:flex-start}
#buffon-game .buffon-canvas-area{width:680px;max-width:min(680px,calc(100vw - 56px));aspect-ratio:680/454;height:auto;flex-shrink:0}
#buffon-game .buffon-sidebar{width:340px;min-width:280px;flex:1 1 300px;display:flex;flex-direction:column;gap:16px}
#buffon-game .g-card{padding:22px 24px}
#buffon-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:14px}
#buffon-game .g-btn{font-size:1.2rem;padding:14px 22px;border-radius:12px}
#buffon-game .g-row{gap:12px;flex-wrap:wrap}
#buffon-game .g-stat{font-size:1.08rem;padding:10px 16px}
#buffon-game .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55;margin-top:12px}
#plinko-game .plinko-layout{display:flex;gap:26px;max-width:1240px;margin:24px auto;padding:0 28px 44px;flex-wrap:wrap;justify-content:center;align-items:flex-start}
#plinko-game .plinko-canvas-area{width:680px;max-width:min(680px,calc(100vw - 56px));aspect-ratio:680/567;height:auto;flex-shrink:0}
#plinko-game .plinko-sidebar{width:340px;min-width:280px;flex:1 1 300px;display:flex;flex-direction:column;gap:16px}
#plinko-game .g-card{padding:22px 24px}
#plinko-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:14px}
#plinko-game .g-btn{font-size:1.2rem;padding:14px 22px;border-radius:12px}
#plinko-game .g-row{gap:12px;flex-wrap:wrap;align-items:center}
#plinko-game .g-stat{font-size:1.08rem;padding:10px 16px}
#plinko-game .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55}
#roulette-game .roul-layout{display:flex;gap:24px;max-width:1000px;margin:24px auto;padding:0 28px 44px;flex-wrap:wrap;justify-content:center;align-items:flex-start}
#roulette-game .roul-wheel-column{flex:0 1 auto;min-width:0;max-width:360px;width:100%}
#roulette-game .roul-sidebar{flex:0 1 300px;width:300px;min-width:260px;max-width:320px;display:flex;flex-direction:column;gap:16px}
#roulette-game .g-card{padding:22px 24px}
#roulette-game .g-card h3{font-size:1.35rem;letter-spacing:1.4px;margin-bottom:14px}
#roulette-game .g-btn{font-size:1.2rem;padding:14px 22px;border-radius:12px}
#roulette-game .g-row{gap:12px;flex-wrap:wrap;align-items:center}
#roulette-game .g-stat{font-size:1.08rem;padding:10px 16px}
#roulette-game .g-msg{font-size:1.75rem;padding:18px 16px;min-height:60px}
#roulette-game .g-info{font-size:1.12rem;padding:18px 20px;line-height:1.55}
#roulette-game .roul-bet-label{font-size:1.15rem;color:var(--text-sub)}
#roulette-game .roul-bet-input{width:96px;padding:10px 16px;border:1px solid var(--border);border-radius:10px;font-family:DM Sans,sans-serif;font-size:1.15rem}
#roulette-game .roul-wheel-wrap{width:100%;max-width:320px;margin:0 auto;position:relative;aspect-ratio:1/1}
#roulette-game .roul-wheel-wrap .roul-canvas{position:absolute;left:0;top:0;width:100%;height:100%;display:block}
#roulette-game #roul-result{min-height:44px;margin:12px 0;font-size:1.4rem;text-align:center}
#roulette-game .roul-res-num{font-size:2.2rem;font-weight:700}
#roulette-game .roul-res-col{font-size:1.05rem;color:var(--text-sub)}
#roulette-game .roul-actions{justify-content:center;margin-top:14px;flex-wrap:nowrap;gap:10px}
#roulette-game .roul-actions .g-btn{font-size:.95rem;padding:8px 18px;border-radius:10px;flex:0 0 auto}
#roulette-game .roul-bet-board{display:flex;flex-direction:column;gap:14px;margin-top:8px}
#roulette-game .bet-chunk-zero{display:flex;justify-content:center}
#roulette-game .bet-chunk-zero .bet-cell{min-width:72px;padding:12px 16px;font-size:1.15rem}
#roulette-game .bet-chunk-red,#roulette-game .bet-chunk-black{display:grid;grid-template-columns:repeat(6,1fr);gap:6px}
#roulette-game .bet-chunk-outside{display:grid;grid-template-columns:repeat(2,1fr);gap:8px}
#roulette-game .bet-cell{padding:10px 6px;font-size:.95rem}
#roulette-game .bet-cell.outside{font-size:.95rem;padding:12px 8px}
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
.bet-cell.outside{background:var(--bg);font-size:.72rem}
@media(max-width:900px){#fourier-game .fourier-layout{flex-direction:column;align-items:center;padding:0 20px 36px}#fourier-game .fourier-sidebar{width:100%;max-width:680px}#buffon-game .buffon-layout{flex-direction:column;align-items:center;padding:0 20px 36px}#buffon-game .buffon-sidebar{width:100%;max-width:680px}#plinko-game .plinko-layout{flex-direction:column;align-items:center;padding:0 20px 36px}#plinko-game .plinko-sidebar{width:100%;max-width:680px}#roulette-game .roul-layout{flex-direction:column;align-items:center;padding:0 20px 36px}#roulette-game .roul-sidebar{width:100%;max-width:360px}#roulette-game .roul-wheel-column{max-width:360px}#roulette-game .roul-actions{flex-wrap:wrap}}
@media(max-width:520px){.hero-title{font-size:1.8rem}.carousel-item{width:220px}.carousel-img{height:160px}.door{width:85px;height:120px}#monty-game .door{width:92px;height:140px}#monty-game .door-content{font-size:2.5rem}#monty-game .door-row{gap:10px;margin:18px 0}}
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
          div(class = "g-row mh-actions",
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
      div(class = "fourier-layout",
          div(class = "g-canvas-wrap g-canvas-wrap-light fourier-canvas-area",
              tags$canvas(id = "mainCanvas"),
              div(class = "canvas-badge", "Draw something!")
          ),
          div(class = "fourier-sidebar",
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
                  div(style = "margin-bottom:12px",
                      div(class = "fourier-slider-head",
                          tags$span("Fourier terms"),
                          tags$span(id = "termsValue", "50")),
                      tags$input(type = "range", min = 1, max = 150, value = 50,
                                 oninput = "updateTerms(this.value)")
                  ),
                  div(
                    div(class = "fourier-slider-head",
                        tags$span("Speed"),
                        tags$span(id = "speedValue", "1x")),
                    tags$input(type = "range", min = 0.2, max = 3, value = 1, step = 0.1,
                               oninput = "updateSpeed(this.value)")
                  )
              ),
              div(class = "g-card", h3("The Math"),
                  div(class = "fourier-formula",
                      HTML("f(t) = &Sigma; c<sub>n</sub> &middot; e<sup>i&middot;n&middot;t</sup>"), br(),
                      tags$span(class = "fourier-formula-note",
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
                                             tags$label(class = "dating-pool-label", "Pool size:"),
                                             tags$input(id = "dat-n", class = "dating-pool-input", type = "number",
                                                        min = 5, max = 100, value = 20),
                                             tags$button(class = "g-btn g-btn-primary", onclick = "datStart()", "Start Game")
      )),
      div(class = "g-card",
          h3(HTML("Candidate &mdash; Pool of <span id='dat-n-show'>20</span>")),
          div(class = "g-msg", id = "dat-msg", "Set your pool size and start!"),
          div(id = "dat-card-area"),
          div(class = "g-row dating-actions",
              tags$button(id = "dat-accept", class = "g-btn g-btn-primary",
                          onclick = "datAccept()", disabled = NA, "Accept"),
              tags$button(id = "dat-reject", class = "g-btn g-btn-secondary",
                          onclick = "datReject()", disabled = NA, "Reject")
          ),
          div(id = "dat-result")
      )
  )
}

plinko_ui <- function() {
  div(id = "plinko-game",
      div(class = "plinko-layout",
          div(class = "g-canvas-wrap g-canvas-wrap-light plinko-canvas-area",
              tags$canvas(id = "plinko-canvas")),
          div(class = "plinko-sidebar",
              div(class = "g-card", h3("Controls"), div(class = "g-row",
                                                        tags$button(class = "g-btn g-btn-primary", onclick = "plinkoDrop(1)", "Drop 1"),
                                                        tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoDrop(10)", "Drop 10"),
                                                        tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoDrop(100)", "Drop 100"),
                                                        tags$button(class = "g-btn g-btn-secondary", onclick = "plinkoReset()", "Reset"),
                                                        div(class = "g-stat", "Balls: ", tags$b(id = "plinko-count", "0"))
              )),
              div(class = "g-info",
                  HTML("<b>The math:</b> Each ball makes independent left/right choices at each peg. By the Central Limit Theorem, the sum follows a normal (bell curve) distribution. More balls = clearer bell shape!"))
          )
      )
  )
}

buffon_ui <- function() {
  div(id = "buffon-game",
      div(class = "buffon-layout",
          div(class = "g-canvas-wrap g-canvas-wrap-light buffon-canvas-area",
              tags$canvas(id = "buffon-canvas")),
          div(class = "buffon-sidebar",
              div(class = "g-card", h3("Drop Needles"),
                  div(class = "g-row",
                      tags$button(class = "g-btn g-btn-primary", onclick = "bufDrop(1)", "Drop 1"),
                      tags$button(class = "g-btn g-btn-secondary", onclick = "bufDrop(10)", "Drop 10"),
                      tags$button(class = "g-btn g-btn-secondary", onclick = "bufDrop(100)", "Drop 100"),
                      tags$button(class = "g-btn g-btn-secondary", onclick = "bufReset()", "Reset")
                  )
              ),
              div(class = "g-card", h3("Results"),
                  div(class = "g-row",
                      div(class = "g-stat", "Needles: ", tags$b(id = "buf-total", "0")),
                      div(class = "g-stat", "Crossings: ", tags$b(id = "buf-hits", "0")),
                      div(class = "g-stat", HTML("&pi; estimate: "), tags$b(id = "buf-pi", "--")),
                      div(class = "g-stat", "Error: ", tags$b(id = "buf-err", "--"))
                  ),
                  div(class = "g-info",
                      HTML("<b>The math:</b> P(crossing) = 2L/(&pi;D), so &pi; = 2L&middot;N/(D&middot;H). The more needles you drop, the closer to &pi; &asymp; 3.14159!"))
              )
          )
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
          div(class = "g-row bj-toolbar",
              div(class = "g-stat", "Chips: $", tags$b(id = "bj-chips", "100")),
              tags$label(class = "bj-bet-label", "Bet: $"),
              tags$input(id = "bj-bet-input", class = "bj-bet-input", type = "number",
                         min = 1, max = 100, value = 10)
          ),
          div(id = "bj-dealer"),
          tags$hr(class = "bj-hr"),
          div(id = "bj-player"),
          div(class = "g-msg", id = "bj-msg", "Place your bet and deal!"),
          div(class = "g-row bj-actions",
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
  black_nums <- sort(setdiff(1:36, reds))
  mk_num_cell <- function(n) {
    cls <- if (n == 0L) "bet-cell green-num"
    else if (n %in% reds) "bet-cell red-num"
    else "bet-cell black-num"
    tags$div(class = cls,
             onclick = paste0("roulBet('number','", n, "',this)"),
             as.character(n))
  }
  chunk_zero <- tags$div(class = "bet-chunk bet-chunk-zero", mk_num_cell(0L))
  chunk_red <- tags$div(class = "bet-chunk bet-chunk-red",
                        lapply(sort(reds), mk_num_cell))
  chunk_black <- tags$div(class = "bet-chunk bet-chunk-black",
                          lapply(black_nums, mk_num_cell))
  chunk_outside <- tags$div(
    class = "bet-chunk bet-chunk-outside",
    tags$div(class = "bet-cell outside", onclick = "roulBet('color','red',this)", "Red"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('color','black',this)", "Black"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('parity','even',this)", "Even"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('parity','odd',this)", "Odd"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('range','1-18',this)", "1-18"),
    tags$div(class = "bet-cell outside", onclick = "roulBet('range','19-36',this)", "19-36")
  )
  
  div(id = "roulette-game",
      div(class = "roul-layout",
          div(class = "roul-wheel-column",
              div(class = "g-card", style = "text-align:center",
                  h3("Wheel"),
                  div(class = "roul-wheel-wrap",
                      tags$canvas(id = "roul-canvas", class = "roul-canvas")),
                  div(id = "roul-result"),
                  div(class = "g-msg", id = "roul-msg", "Place a bet!")
              )
          ),
          div(class = "roul-sidebar",
              div(class = "g-card",
                  h3("Place Your Bet"),
                  div(class = "g-row",
                      div(class = "g-stat", "Chips: $", tags$b(id = "roul-chips", "100")),
                      tags$label(class = "roul-bet-label", "Bet: $"),
                      tags$input(id = "roul-bet-amt", class = "roul-bet-input", type = "number",
                                 min = 1, max = 100, value = 10)
                  ),
                  div(class = "roul-bet-board",
                      chunk_zero, chunk_red, chunk_black, chunk_outside),
                  div(class = "g-row roul-actions",
                      tags$button(id = "roul-spin", class = "g-btn g-btn-primary",
                                  onclick = "roulSpin()", "Spin!"),
                      tags$button(class = "g-btn g-btn-secondary", onclick = "roulReset()", "Reset")
                  )
              ),
              div(class = "g-info",
                  HTML("<b>The math:</b> European roulette has 37 slots (0&ndash;36). A number bet pays 35:1 but true odds are 36:1 &mdash; that 2.7% gap is the house edge. Over many spins, the house always wins!"))
          )
      )
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
                  style = paste0("--img-tint:", g$color, ";"),
                  tags$img(src = g$thumb, alt = g$title, class = "carousel-thumb")),
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
      return(div(class = "fourier-coeff-placeholder",
                 "Draw a shape to see coefficients"))
    }
    tn <- min(8, nrow(fc))
    rows <- lapply(seq_len(tn), function(i) {
      tags$tr(tags$td(fc$freq[i]), tags$td(fc$amp[i]), tags$td(fc$phase[i]))
    })
    extra <- if (nrow(fc) > tn) {
      div(class = "fourier-coeff-more",
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