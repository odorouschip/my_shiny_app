/* ═══ NAV ═══ */
function goToGame(id){Shiny.setInputValue("nav_target",id,{priority:"event"});}
function goHome(){Shiny.setInputValue("nav_target","home",{priority:"event"});}

$(document).on("shiny:value",function(e){
  if(e.name!=="main_view")return;
  setTimeout(function(){
    if(document.getElementById("monty-game"))initMontyHall();
    if(document.getElementById("fourier-game"))initFourier();
    if(document.getElementById("dating-game"))initDating();
    if(document.getElementById("plinko-game"))initPlinko();
    if(document.getElementById("buffon-game"))initBuffon();
    if(document.getElementById("poker-game"))initPoker();
    if(document.getElementById("blackjack-game"))initBlackjack();
    if(document.getElementById("roulette-game"))initRoulette();
  },150);
});

/* ══════════ 1. MONTY HALL ══════════ */
function initMontyHall(){
  var carDoor,picked,revealed,phase;
  var stats={sw:0,st:0,swin:0,stwin:0};
  var msg=document.getElementById("mh-msg");
  var doors=document.querySelectorAll(".door");
  newRound();
  function newRound(){
    phase="pick";carDoor=Math.floor(Math.random()*3);picked=-1;revealed=-1;
    doors.forEach(function(d){d.className="door";d.querySelector(".door-content").textContent="?";});
    msg.textContent="Pick a door!";
    document.getElementById("mh-switch").style.display="none";
    document.getElementById("mh-stay").style.display="none";
  }
  doors.forEach(function(d,i){d.addEventListener("click",function(){pickDoor(i);});});
  function pickDoor(i){
    if(phase!=="pick")return;
    picked=i;phase="reveal";
    doors[i].style.borderColor="var(--accent)";
    var opts=[];for(var j=0;j<3;j++)if(j!==picked&&j!==carDoor)opts.push(j);
    revealed=opts[Math.floor(Math.random()*opts.length)];
    doors[revealed].classList.add("opened");
    doors[revealed].querySelector(".door-content").textContent="\uD83D\uDC10";
    msg.textContent="Door "+(revealed+1)+" has a goat! Switch or stay?";
    document.getElementById("mh-switch").style.display="inline-block";
    document.getElementById("mh-stay").style.display="inline-block";
  }
  window.mhSwitch=function(){
    if(phase!=="reveal")return;
    var np=[0,1,2].filter(function(j){return j!==picked&&j!==revealed;})[0];
    resolve(np,true);
  };
  window.mhStay=function(){
    if(phase!=="reveal")return;
    resolve(picked,false);
  };
  function resolve(final,switched){
    phase="done";var won=final===carDoor;
    doors.forEach(function(d,i){
      d.classList.add("disabled");
      if(i===carDoor){d.classList.add("opened","winner");d.querySelector(".door-content").textContent="\uD83D\uDE97";}
      else if(!d.classList.contains("opened")){d.classList.add("opened");d.querySelector(".door-content").textContent="\uD83D\uDC10";}
    });
    if(switched){stats.st++;if(won)stats.swin++;}
    else{stats.sw++;if(won)stats.stwin++;}
    msg.innerHTML=won
      ?'<span class="mh-win">\uD83C\uDF89 You won the car!</span>'
      :'<span class="mh-lose">Goat! Car was behind door '+(carDoor+1)+"</span>";
    var sp=stats.st?((stats.swin/stats.st)*100).toFixed(0):"--";
    var stp=stats.sw?((stats.stwin/stats.sw)*100).toFixed(0):"--";
    document.getElementById("mh-stats").innerHTML=
      "<b>Switch:</b> "+stats.swin+"/"+stats.st+" ("+sp+"%) &nbsp; <b>Stay:</b> "+stats.stwin+"/"+stats.sw+" ("+stp+"%)";
    document.getElementById("mh-switch").style.display="none";
    document.getElementById("mh-stay").style.display="none";
  }
  window.mhNew=newRound;
}

/* ══════════ 2. FOURIER ══════════ */
function initFourier(){
  var canvas=document.getElementById("mainCanvas");
  if(!canvas)return;
  var ctx=canvas.getContext("2d");
  var W=680,H=680;canvas.width=W;canvas.height=H;
  var dp=[],isD=false,mode="draw",coeffs=[];
  var nTerms=50,aSpeed=1,time=0,trace=[],af=null;
  var totS=400,sC=0,origP=[];
  var SN=33,nearS=false;
  var badge=document.querySelector(".canvas-badge");

  function gp(e){
    var r=canvas.getBoundingClientRect();
    var sx=W/r.width,sy=H/r.height;
    if(e.touches)return{x:(e.touches[0].clientX-r.left)*sx,y:(e.touches[0].clientY-r.top)*sy};
    return{x:(e.clientX-r.left)*sx,y:(e.clientY-r.top)*sy};
  }
  function ds(p){
    if(!dp.length)return 1e9;
    var s=dp[0];return Math.hypot(p.x-s.x,p.y-s.y);
  }
  function sD(e){
    if(mode!=="draw")return;e.preventDefault();
    isD=true;dp=[];trace=[];nearS=false;dp.push(gp(e));
  }
  function mD(e){
    if(!isD)return;e.preventDefault();
    var p=gp(e);dp.push(p);nearS=dp.length>30&&ds(p)<SN;rD();
  }
  function eD(e){
    if(!isD)return;e.preventDefault();isD=false;nearS=false;
    if(dp.length>10){
      dp.push({x:dp[0].x,y:dp[0].y});rD();
      badge.textContent="Ready \u2014 hit Animate!";
      document.getElementById("animateBtn").disabled=false;
    }else{badge.textContent="Draw something bigger!";}
  }
  canvas.addEventListener("mousedown",sD);
  canvas.addEventListener("mousemove",mD);
  canvas.addEventListener("mouseup",eD);
  canvas.addEventListener("mouseleave",eD);
  canvas.addEventListener("touchstart",sD,{passive:false});
  canvas.addEventListener("touchmove",mD,{passive:false});
  canvas.addEventListener("touchend",eD,{passive:false});

  function rD(){
    ctx.clearRect(0,0,W,H);if(dp.length<2)return;
    ctx.beginPath();ctx.moveTo(dp[0].x,dp[0].y);
    for(var i=1;i<dp.length;i++)ctx.lineTo(dp[i].x,dp[i].y);
    ctx.strokeStyle="#000";ctx.lineWidth=2.5;ctx.lineCap="round";ctx.lineJoin="round";ctx.stroke();
    if(isD&&dp.length>1){
      var s=dp[0];
      ctx.beginPath();ctx.arc(s.x,s.y,SN,0,2*Math.PI);
      ctx.strokeStyle=nearS?"rgba(212,100,59,.7)":"rgba(0,0,0,.2)";
      ctx.lineWidth=nearS?2:1;ctx.setLineDash([4,4]);ctx.stroke();ctx.setLineDash([]);
      ctx.beginPath();ctx.arc(s.x,s.y,5,0,2*Math.PI);
      ctx.fillStyle=nearS?"#d4643b":"rgba(0,0,0,.45)";ctx.fill();
    }
    if(isD&&dp.length>30)badge.textContent=nearS?"Release to close!":"Loop back to close";
  }

  window.loadPreset=function(shape){
    stopA();mode="draw";dp=[];trace=[];
    var cx=W/2,cy=H/2,r=220,N=300,i,t;
    if(shape==="circle"){
      for(i=0;i<=N;i++){t=i/N*2*Math.PI;dp.push({x:cx+r*Math.cos(t),y:cy+r*Math.sin(t)});}
    }else if(shape==="star"){
      for(i=0;i<=N;i++){t=i/N*2*Math.PI;var R=r*(.5+.5*Math.abs(Math.cos(2.5*t)));dp.push({x:cx+R*Math.cos(t),y:cy+R*Math.sin(t)});}
    }else if(shape==="heart"){
      for(i=0;i<=N;i++){t=i/N*2*Math.PI;var hx=16*Math.pow(Math.sin(t),3);var hy=-(13*Math.cos(t)-5*Math.cos(2*t)-2*Math.cos(3*t)-Math.cos(4*t));dp.push({x:cx+hx*(r/18),y:cy+hy*(r/18)});}
    }else if(shape==="figure8"){
      for(i=0;i<=N;i++){t=i/N*2*Math.PI;dp.push({x:cx+r*Math.sin(t),y:cy+r*.6*Math.sin(2*t)});}
    }
    rD();badge.textContent="Ready \u2014 hit Animate!";
    document.getElementById("animateBtn").disabled=false;
    document.querySelectorAll(".f-btn-preset").forEach(function(b){b.classList.remove("active");});
  };

  window.startAnimation=function(){
    if(dp.length<10)return;
    var pts=dp.map(function(p){return{x:p.x-W/2,y:p.y-H/2};});
    Shiny.setInputValue("drawn_points",JSON.stringify(pts),{priority:"event"});
    origP=dp.slice();badge.textContent="Computing\u2026";
    document.getElementById("animateBtn").disabled=true;
  };

  window.clearCanvas=function(){
    stopA();mode="draw";dp=[];trace=[];origP=[];coeffs=[];
    ctx.clearRect(0,0,W,H);badge.textContent="Draw something!";
    document.getElementById("animateBtn").disabled=true;
    document.querySelectorAll(".f-btn-preset").forEach(function(b){b.classList.remove("active");});
    Shiny.setInputValue("clear_signal",Math.random(),{priority:"event"});
  };

  function stopA(){if(af)cancelAnimationFrame(af);af=null;mode="draw";}

  Shiny.addCustomMessageHandler("fourier_coeffs", function(data){
    coeffs = data;
    mode = "animate";
    time = 0;
    trace = [];
    badge.textContent = "Animating with " + Math.min(nTerms, coeffs.length) + " terms";
    anim();
  });

  function anim(){
    ctx.clearRect(0,0,W,H);

    if(origP.length>1){
      ctx.beginPath();
      ctx.moveTo(origP[0].x,origP[0].y);
      for(var i=1;i<origP.length;i++) ctx.lineTo(origP[i].x,origP[i].y);
      ctx.strokeStyle="rgba(0,0,0,.12)";
      ctx.lineWidth=1.5;
      ctx.stroke();
    }

    if(trace.length>1){
      ctx.beginPath();
      ctx.moveTo(trace[0].x+W/2,trace[0].y+H/2);
      for(var i=1;i<trace.length;i++) ctx.lineTo(trace[i].x+W/2,trace[i].y+H/2);
      ctx.strokeStyle="#ffaa00";
      ctx.lineWidth=2.5;
      ctx.lineCap="round";
      ctx.stroke();
    }

    var n = Math.min(nTerms, coeffs.length), x = 0, y = 0;
    for(var i=0;i<n;i++){
      var c = coeffs[i], px = x, py = y;
      var ang = c.freq * time + c.phase;
      x += c.amp * Math.cos(ang);
      y += c.amp * Math.sin(ang);

      ctx.beginPath();
      ctx.arc(px+W/2, py+H/2, c.amp, 0, 2*Math.PI);
      ctx.strokeStyle = "rgba(100,180,255," + Math.max(.06,.35-i*.005) + ")";
      ctx.lineWidth = 1;
      ctx.stroke();

      ctx.beginPath();
      ctx.moveTo(px+W/2, py+H/2);
      ctx.lineTo(x+W/2, y+H/2);
      ctx.strokeStyle = "rgba(170,128,255," + Math.max(.15,.7-i*.01) + ")";
      ctx.lineWidth = 1.2;
      ctx.stroke();
    }

    ctx.beginPath();
    ctx.arc(x+W/2, y+H/2, 3.5, 0, 2*Math.PI);
    ctx.fillStyle = "#ff4488";
    ctx.fill();

    trace.push({x:x, y:y});

    var cycle = 2 * Math.PI;
    var baseStep = cycle / totS;
    time += aSpeed * baseStep;

    if (time >= cycle) {
      time -= cycle;
      trace = [];
    }

    af = requestAnimationFrame(anim);
  }

  window.updateTerms=function(v){
    nTerms=parseInt(v);document.getElementById("termsValue").textContent=v;
    if(mode==="animate")badge.textContent="Animating with "+Math.min(nTerms,coeffs.length)+" terms";
  };
  window.updateSpeed=function(v){
    aSpeed=parseFloat(v);document.getElementById("speedValue").textContent=v+"x";
  };
}

/* ══════════ 3. DATING / SECRETARY ══════════ */
function initDating(){
  var N=20,cands=[],idx=0,bestSoFar=0,phase="setup",optSkip=0;
  var msg=document.getElementById("dat-msg");

  window.datStart=function(){
    N=parseInt(document.getElementById("dat-n").value)||20;
    document.getElementById("dat-n-show").textContent=N;
    cands=[];
    for(var i=0;i<N;i++)cands.push(Math.floor(Math.random()*100)+1);
    idx=0;bestSoFar=0;optSkip=Math.max(1,Math.round(N/Math.E));phase="explore";
    document.getElementById("dat-card-area").innerHTML="";
    document.getElementById("dat-accept").disabled=true;
    document.getElementById("dat-reject").disabled=false;
    document.getElementById("dat-result").innerHTML="";
    showCand();
  };

  function showCand(){
    if(idx>=N){
      phase="done";msg.textContent="You ran out of candidates!";
      document.getElementById("dat-accept").disabled=true;
      document.getElementById("dat-reject").disabled=true;
      showRes(-1);return;
    }
    var score=cands[idx],inEx=idx<optSkip;
    var icon=inEx?"\uD83D\uDD0D":"\uD83D\uDCA1";
    document.getElementById("dat-card-area").innerHTML=
      '<div class="dating-cand-wrap">'+
      '<div class="dating-cand-icon">'+icon+'</div>'+
      '<div class="dating-cand-sub">Candidate '+(idx+1)+' of '+N+'</div>'+
      '<div class="dating-cand-score">Score: '+score+'</div>'+
      '<div class="dating-cand-best">Best seen so far: '+Math.round(bestSoFar)+'</div></div>';
    if(inEx){
      msg.textContent="Exploring phase (skip first "+optSkip+"). Must reject.";
      document.getElementById("dat-accept").disabled=true;
    }else{
      msg.textContent="Selection phase! Accept if they seem best.";
      document.getElementById("dat-accept").disabled=false;
    }
  }

  window.datReject=function(){
    if(phase!=="explore"&&phase!=="select")return;
    if(cands[idx]>bestSoFar)bestSoFar=cands[idx];
    idx++;if(idx>=optSkip)phase="select";showCand();
  };
  window.datAccept=function(){
    if(phase!=="select")return;phase="done";
    document.getElementById("dat-accept").disabled=true;
    document.getElementById("dat-reject").disabled=true;
    showRes(idx);
  };

  function showRes(choice){
    var best=0;
    for(var i=1;i<cands.length;i++)if(cands[i]>cands[best])best=i;
    var bScore=cands[best],html;
    if(choice===-1){
      html='<div class="dating-result-msg dating-result-fail">You rejected everyone! Best was #'+(best+1)+' (score '+bScore+').</div>';
    }else if(choice===best){
      html='<div class="dating-result-msg dating-result-win">\uD83C\uDF89 Perfect! You picked the best (score '+cands[choice]+')!</div>';
    }else{
      html='<div class="dating-result-msg dating-result-mid">You picked score '+cands[choice]+'. Best was #'+(best+1)+' (score '+bScore+').</div>';
    }
    html+='<div class="g-info"><b>Optimal stopping theory:</b> Reject the first N/e \u2248 '+optSkip+' candidates, then pick the next one better than all previous. This gives ~37% chance of finding the absolute best!</div>';
    document.getElementById("dat-result").innerHTML=html;
    msg.textContent="Round over!";
  }
}

/* ══════════ 4. PLINKO ══════════ */
function initPlinko(){
  var canvas=document.getElementById("plinko-canvas");
  if(!canvas)return;
  var ctx=canvas.getContext("2d");
  var W=680,H=567;canvas.width=W;canvas.height=H;
  var ROWS=10,BINS=ROWS+1;
  var pegSX=W/(ROWS+2),pegSY=(H-110)/(ROWS+1),pegY0=50;
  var bins=new Array(BINS).fill(0),balls=[],total=0;

  function drawPegs(){
    for(var r=0;r<ROWS;r++){
      var np=r+2,ox=(W-(np-1)*pegSX)/2;
      for(var c=0;c<np;c++){
        var px=ox+c*pegSX,py=pegY0+(r+1)*pegSY;
        ctx.beginPath();ctx.arc(px,py,3.5,0,2*Math.PI);
        ctx.fillStyle="#8a8078";ctx.fill();
      }
    }
  }

  function drawBins(){
    var mx=1;for(var i=0;i<BINS;i++)if(bins[i]>mx)mx=bins[i];
    var bw=W/BINS,base=H-5,mh=92;
    for(var i=0;i<BINS;i++){
      var bh=bins[i]/mx*mh,bx=i*bw+2;
      ctx.fillStyle="rgba(212,100,59,0.45)";
      ctx.fillRect(bx,base-bh,bw-4,bh);
      if(bins[i]>0){
        ctx.fillStyle="#555";ctx.font="12px DM Sans";ctx.textAlign="center";
        ctx.fillText(bins[i],bx+(bw-4)/2,base-bh-4);
      }
    }
  }

  function drop(n){
    for(var k=0;k<n;k++){
      var bin=0,path=[{x:W/2,y:10}];
      for(var r=0;r<ROWS;r++){
        if(Math.random()>.5)bin++;
        var np=r+3,ox=(W-(np-1)*pegSX)/2;
        path.push({x:ox+bin*pegSX,y:pegY0+(r+1)*pegSY+5});
      }
      var bw=W/BINS;
      path.push({x:bin*bw+bw/2,y:H-12});
      bins[bin]++;total++;
      balls.push({path:path,step:0});
    }
    document.getElementById("plinko-count").textContent=total;
  }

  function render(){
    ctx.clearRect(0,0,W,H);drawPegs();drawBins();
    var still=[];
    balls.forEach(function(b){
      if(b.step<b.path.length){
        var p=b.path[b.step];
        ctx.beginPath();ctx.arc(p.x,p.y,5.5,0,2*Math.PI);
        ctx.fillStyle="#d4643b";ctx.fill();
        b.step++;still.push(b);
      }
    });
    balls=still;
    requestAnimationFrame(render);
  }

  window.plinkoDrop=function(n){drop(n||1);};
  window.plinkoReset=function(){
    bins=new Array(BINS).fill(0);balls=[];total=0;
    document.getElementById("plinko-count").textContent="0";
  };
  render();
}

/* ══════════ 5. BUFFON NEEDLE ══════════ */
function initBuffon(){
  var canvas=document.getElementById("buffon-canvas");
  if(!canvas)return;
  var ctx=canvas.getContext("2d");
  var W=680,H=454;canvas.width=W;canvas.height=H;
  var D=60*(W/600),L=40*(W/600);
  var needles=[],hits=0,total=0;

  function drawLines(){
    ctx.strokeStyle="#c9bfb4";ctx.lineWidth=1;
    for(var y=D;y<H;y+=D){ctx.beginPath();ctx.moveTo(0,y);ctx.lineTo(W,y);ctx.stroke();}
  }
  function drawN(){
    needles.forEach(function(n){
      ctx.beginPath();ctx.moveTo(n.x1,n.y1);ctx.lineTo(n.x2,n.y2);
      ctx.strokeStyle=n.hit?"#e74c3c":"rgba(110,98,88,0.55)";
      ctx.lineWidth=n.hit?2:1.2;ctx.stroke();
    });
  }
  function render(){ctx.clearRect(0,0,W,H);drawLines();drawN();upd();}

  function dropOne(){
    var cx=Math.random()*W,cy=Math.random()*H;
    var ang=Math.random()*Math.PI;
    var dx=L/2*Math.cos(ang),dy=L/2*Math.sin(ang);
    var x1=cx-dx,y1=cy-dy,x2=cx+dx,y2=cy+dy;
    var hit=false;
    for(var ly=D;ly<H;ly+=D){
      if((y1<=ly&&y2>=ly)||(y2<=ly&&y1>=ly)){hit=true;break;}
    }
    needles.push({x1:x1,y1:y1,x2:x2,y2:y2,hit:hit});
    total++;if(hit)hits++;
  }

  function upd(){
    var pi=total>0&&hits>0?(2*L*total)/(D*hits):0;
    document.getElementById("buf-total").textContent=total;
    document.getElementById("buf-hits").textContent=hits;
    document.getElementById("buf-pi").textContent=hits>0?pi.toFixed(4):"--";
    document.getElementById("buf-err").textContent=hits>0?Math.abs(pi-Math.PI).toFixed(4):"--";
  }

  window.bufDrop=function(n){for(var i=0;i<(n||1);i++)dropOne();render();};
  window.bufReset=function(){needles=[];hits=0;total=0;render();};
  render();
}

/* ══════════ 6. POKER ══════════ */
function initPoker(){
  var suits=["\u2660","\u2665","\u2666","\u2663"];
  var ranks=["2","3","4","5","6","7","8","9","10","J","Q","K","A"];
  var stats={},totalH=0;

  function mkDeck(){
    var d=[];suits.forEach(function(s){ranks.forEach(function(r){d.push({rank:r,suit:s});});});return d;
  }
  function shuf(a){
    for(var i=a.length-1;i>0;i--){var j=Math.floor(Math.random()*(i+1));var t=a[i];a[i]=a[j];a[j]=t;}return a;
  }
  function rv(r){
    if(r==="J")return 11;if(r==="Q")return 12;if(r==="K")return 13;if(r==="A")return 14;return parseInt(r);
  }

  function evalH(hand){
    var vals=hand.map(function(c){return rv(c.rank);}).sort(function(a,b){return a-b;});
    var ss=new Set(hand.map(function(c){return c.suit;}));
    var isF=ss.size===1;
    var isS=(vals[4]-vals[0]===4&&new Set(vals).size===5)||(vals.join(",")=="2,3,4,5,14");
    var cnt={};vals.forEach(function(v){cnt[v]=(cnt[v]||0)+1;});
    var fr=Object.values(cnt).sort(function(a,b){return b-a;});
    if(isF&&isS&&vals[0]===10)return"Royal Flush";
    if(isF&&isS)return"Straight Flush";
    if(fr[0]===4)return"Four of a Kind";
    if(fr[0]===3&&fr[1]===2)return"Full House";
    if(isF)return"Flush";
    if(isS)return"Straight";
    if(fr[0]===3)return"Three of a Kind";
    if(fr[0]===2&&fr[1]===2)return"Two Pair";
    if(fr[0]===2)return"One Pair";
    return"High Card";
  }

  function cHTML(c){
    var red=c.suit==="\u2665"||c.suit==="\u2666";
    return'<div class="playing-card '+(red?"red":"black")+'"><span>'+c.rank+'</span><span class="card-suit">'+c.suit+'</span></div>';
  }

  window.pokerDeal=function(){
    var d=shuf(mkDeck()),h=d.slice(0,5),r=evalH(h);
    stats[r]=(stats[r]||0)+1;totalH++;
    document.getElementById("poker-cards").innerHTML=h.map(cHTML).join("");
    document.getElementById("poker-result").innerHTML="<b>"+r+"</b>";
    upS();
  };

  window.pokerDeal1000=function(){
    for(var i=0;i<1000;i++){
      var d=shuf(mkDeck()),h=d.slice(0,5),r=evalH(h);
      stats[r]=(stats[r]||0)+1;totalH++;
    }
    document.getElementById("poker-result").innerHTML="<b>Dealt 1,000 hands!</b>";
    upS();
  };

  window.pokerReset=function(){
    stats={};totalH=0;
    document.getElementById("poker-cards").innerHTML="";
    document.getElementById("poker-result").innerHTML="";
    upS();
  };

  function upS(){
    var order=["Royal Flush","Straight Flush","Four of a Kind","Full House","Flush","Straight","Three of a Kind","Two Pair","One Pair","High Card"];
    var theo=[0.000154,0.00139,0.0240,0.1441,0.1965,0.3925,2.1128,4.7539,42.2569,50.1177];
    var html='<table class="poker-stats-table">';
    html+='<tr><th style="text-align:left;border-bottom:1px solid var(--border)">Hand</th><th style="border-bottom:1px solid var(--border)">Count</th><th style="border-bottom:1px solid var(--border)">Your %</th><th style="border-bottom:1px solid var(--border)">True %</th></tr>';
    order.forEach(function(h,i){
      var c=stats[h]||0;
      var pct=totalH?(c/totalH*100).toFixed(2):"0.00";
      html+='<tr><td>'+h+'</td><td style="text-align:center">'+c+'</td><td style="text-align:center;font-family:JetBrains Mono,monospace">'+pct+'%</td><td style="text-align:center;color:var(--text-sub);font-family:JetBrains Mono,monospace">'+theo[i].toFixed(2)+'%</td></tr>';
    });
    html+='</table><div class="poker-stats-total">Total: '+totalH+' hands</div>';
    document.getElementById("poker-stats").innerHTML=html;
  }
  upS();
}

/* ══════════ 7. BLACKJACK ══════════ */
function initBlackjack(){
  var suits=["\u2660","\u2665","\u2666","\u2663"];
  var ranks=["2","3","4","5","6","7","8","9","10","J","Q","K","A"];
  var deck,pH,dH,phase,chips=100;

  function mkD(){
    var d=[];for(var k=0;k<6;k++)suits.forEach(function(s){ranks.forEach(function(r){d.push({rank:r,suit:s});});});return d;
  }
  function shuf(a){for(var i=a.length-1;i>0;i--){var j=Math.floor(Math.random()*(i+1));var t=a[i];a[i]=a[j];a[j]=t;}return a;}
  function cv(r){if(r==="J"||r==="Q"||r==="K")return 10;if(r==="A")return 11;return parseInt(r);}
  function ht(h){var t=0,a=0;h.forEach(function(c){t+=cv(c.rank);if(c.rank==="A")a++;});while(t>21&&a>0){t-=10;a--;}return t;}

  function cH(c,hid){
    if(hid)return'<div class="playing-card" style="background:#2c3e50;color:#2c3e50"><span>?</span><span class="card-suit">?</span></div>';
    var red=c.suit==="\u2665"||c.suit==="\u2666";
    return'<div class="playing-card '+(red?"red":"black")+'"><span>'+c.rank+'</span><span class="card-suit">'+c.suit+'</span></div>';
  }

  function render(){
    var hD=phase==="player";
    document.getElementById("bj-dealer").innerHTML=
      '<div class="bj-hand-label">Dealer'+(hD?" (???)":(" ("+ht(dH)+")"))+'</div><div class="card-row">'+dH.map(function(c,i){return cH(c,hD&&i===1);}).join("")+'</div>';
    document.getElementById("bj-player").innerHTML=
      '<div class="bj-hand-label">You ('+ht(pH)+')</div><div class="card-row">'+pH.map(function(c){return cH(c,false);}).join("")+'</div>';
    document.getElementById("bj-chips").textContent=chips;
  }

  window.bjDeal=function(){
    if(chips<=0){document.getElementById("bj-msg").textContent="Out of chips! Reset.";return;}
    var bet=parseInt(document.getElementById("bj-bet-input").value)||10;
    if(bet>chips)bet=chips;
    if(!deck||deck.length<20)deck=shuf(mkD());
    pH=[deck.pop(),deck.pop()];dH=[deck.pop(),deck.pop()];phase="player";
    document.getElementById("bj-msg").textContent="Hit or Stand?";
    document.getElementById("bj-hit").disabled=false;
    document.getElementById("bj-stand").disabled=false;
    document.getElementById("bj-deal").disabled=true;
    render();
    if(ht(pH)===21)window.bjStand();
  };

  window.bjHit=function(){
    if(phase!=="player")return;
    pH.push(deck.pop());render();
    if(ht(pH)>21)endBJ("bust");
    else if(ht(pH)===21)window.bjStand();
  };

  window.bjStand=function(){
    if(phase!=="player")return;
    phase="dealer";
    while(ht(dH)<17)dH.push(deck.pop());
    phase="done";render();
    var pt=ht(pH),dt=ht(dH);
    if(pt>21)endBJ("bust");
    else if(dt>21)endBJ("dBust");
    else if(pt>dt)endBJ("win");
    else if(dt>pt)endBJ("lose");
    else endBJ("push");
  };

  function endBJ(r){
    phase="done";
    var bet=parseInt(document.getElementById("bj-bet-input").value)||10;
    if(bet>chips+bet)bet=chips;
    document.getElementById("bj-hit").disabled=true;
    document.getElementById("bj-stand").disabled=true;
    document.getElementById("bj-deal").disabled=false;
    render();
    var msgs={
      bust:"\uD83D\uDCA5 Bust! -$"+bet,
      dBust:"\uD83C\uDF89 Dealer busts! +$"+bet,
      win:"\uD83C\uDF89 You win! +$"+bet,
      lose:"Dealer wins. -$"+bet,
      push:"\uD83E\uDD1D Push!"
    };
    if(r==="win"||r==="dBust")chips+=bet;
    else if(r==="bust"||r==="lose")chips-=bet;
    document.getElementById("bj-msg").textContent=msgs[r];
    document.getElementById("bj-chips").textContent=chips;
  }

  window.bjReset=function(){
    chips=100;deck=shuf(mkD());pH=[];dH=[];phase="idle";
    document.getElementById("bj-msg").textContent="Place your bet and deal!";
    document.getElementById("bj-dealer").innerHTML="";
    document.getElementById("bj-player").innerHTML="";
    document.getElementById("bj-hit").disabled=true;
    document.getElementById("bj-stand").disabled=true;
    document.getElementById("bj-deal").disabled=false;
    document.getElementById("bj-chips").textContent=chips;
  };
  deck=shuf(mkD());
}

/* ══════════ 8. ROULETTE ══════════ */
function initRoulette(){
  var canvas=document.getElementById("roul-canvas");
  if(!canvas)return;
  var ctx=canvas.getContext("2d");
  var SZ=320;
  canvas.width=SZ;canvas.height=SZ;
  var CX=160,CY=160,R=140;
  var nums=[0,32,15,19,4,21,2,25,17,34,6,27,13,36,11,30,8,23,10,5,24,16,33,1,20,14,31,9,22,18,29,7,28,12,35,3,26];
  var reds=[1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36];
  var angle=0,spinning=false,chips=100;
  var curBet={type:null,value:null,amount:10},spinVel=0;

  function nc(n){
    if(n===0)return"#27ae60";
    return reds.indexOf(n)>=0?"#c0392b":"#2c3e50";
  }

  function drawW(){
    ctx.clearRect(0,0,SZ,SZ);
    var sa=2*Math.PI/nums.length;
    nums.forEach(function(n,i){
      var a1=angle+i*sa,a2=a1+sa;
      ctx.beginPath();ctx.moveTo(CX,CY);ctx.arc(CX,CY,R,a1,a2);ctx.closePath();
      ctx.fillStyle=nc(n);ctx.fill();ctx.strokeStyle="#111";ctx.lineWidth=1;ctx.stroke();
      var mid=a1+sa/2,tx=CX+Math.cos(mid)*(R-22),ty=CY+Math.sin(mid)*(R-22);
      ctx.save();ctx.translate(tx,ty);ctx.rotate(mid+Math.PI/2);
      ctx.fillStyle="#fff";ctx.font="bold 10px DM Sans";ctx.textAlign="center";
      ctx.fillText(n,0,0);ctx.restore();
    });
    ctx.beginPath();ctx.arc(CX,CY,20,0,2*Math.PI);
    ctx.fillStyle="#faf7f2";ctx.fill();ctx.strokeStyle="#ddd";ctx.lineWidth=2;ctx.stroke();
    ctx.beginPath();ctx.moveTo(CX,CY-R-8);ctx.lineTo(CX-8,CY-R+8);ctx.lineTo(CX+8,CY-R+8);ctx.closePath();
    ctx.fillStyle="#d4643b";ctx.fill();
  }

  function getRes(){
    var sa=2*Math.PI/nums.length;
    var pa=(-Math.PI/2-angle)%(2*Math.PI);
    if(pa<0)pa+=2*Math.PI;
    return nums[Math.floor(pa/sa)%nums.length];
  }

  function anim(){
    if(!spinning)return;
    angle+=spinVel;spinVel*=0.985;
    if(spinVel<0.001){
      spinning=false;
      var res=getRes();
      var col=res===0?"green":(reds.indexOf(res)>=0?"red":"black");
      document.getElementById("roul-result").innerHTML=
        '<span class="roul-res-num" style="color:'+nc(res)+'">'+res+'</span> '+
        '<span class="roul-res-col">'+col+'</span>';
      resolveBet(res);
      document.getElementById("roul-spin").disabled=false;
    }
    drawW();
    if(spinning)requestAnimationFrame(anim);
  }

  window.roulSpin=function(){
    if(spinning)return;
    if(!curBet.type){document.getElementById("roul-msg").textContent="Place a bet first!";return;}
    curBet.amount=parseInt(document.getElementById("roul-bet-amt").value)||10;
    if(curBet.amount>chips){document.getElementById("roul-msg").textContent="Not enough chips!";return;}
    spinning=true;spinVel=0.2+Math.random()*0.3;
    document.getElementById("roul-spin").disabled=true;
    document.getElementById("roul-result").innerHTML="";
    document.getElementById("roul-msg").textContent="Spinning\u2026";
    anim();
  };

  window.roulBet=function(type,value,el){
    curBet={type:type,value:value,amount:parseInt(document.getElementById("roul-bet-amt").value)||10};
    document.querySelectorAll(".bet-cell").forEach(function(c){c.classList.remove("selected");});
    if(el)el.classList.add("selected");
    document.getElementById("roul-msg").textContent="Bet: "+type+" "+value+". Spin!";
  };

  function resolveBet(num){
    var col=num===0?"green":(reds.indexOf(num)>=0?"red":"black");
    var isEven=num>0&&num%2===0;
    var won=false,payout=0;
    if(curBet.type==="number"&&parseInt(curBet.value)===num){won=true;payout=curBet.amount*35;}
    else if(curBet.type==="color"&&curBet.value===col){won=true;payout=curBet.amount;}
    else if(curBet.type==="parity"&&curBet.value==="even"&&isEven){won=true;payout=curBet.amount;}
    else if(curBet.type==="parity"&&curBet.value==="odd"&&num>0&&!isEven){won=true;payout=curBet.amount;}
    else if(curBet.type==="range"&&curBet.value==="1-18"&&num>=1&&num<=18){won=true;payout=curBet.amount;}
    else if(curBet.type==="range"&&curBet.value==="19-36"&&num>=19){won=true;payout=curBet.amount;}
    if(won){chips+=payout;document.getElementById("roul-msg").textContent="\uD83C\uDF89 Won $"+payout+"!";}
    else{chips-=curBet.amount;document.getElementById("roul-msg").textContent="Lost $"+curBet.amount+".";}
    document.getElementById("roul-chips").textContent=chips;
    curBet={type:null,value:null,amount:10};
    document.querySelectorAll(".bet-cell").forEach(function(c){c.classList.remove("selected");});
  }

  window.roulReset=function(){
    chips=100;curBet={type:null,value:null,amount:10};
    document.getElementById("roul-chips").textContent=100;
    document.getElementById("roul-msg").textContent="Place a bet!";
    document.getElementById("roul-result").innerHTML="";
    drawW();
  };
  drawW();
}
