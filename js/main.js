// ------------ main.js (smart eval + fast search, same UI) ------------
(function () {
  // document.addEventListener('DOMContentLoaded', init);
  $(init);

  function init() {
    // ---------- Globals / UI ----------
    var STACK_SIZE = 100;
    var board = null;
    var $board = $('#myBoard');
    if ($board.length === 0) {
      console.error('[AI] #myBoard not found in DOM.');
      return;
    }

    var game = new Chess();
    var globalSum = 0; // always from Black's pov (>0 means Black better)
    var whiteSquareGrey = '#a9a9a9';
    var blackSquareGrey = '#696969';

    var squareClass = 'square-55d63';
    var squareToHighlight = null;
    var colorToHighlight = null;
    var positionCount = 0;
    var timer = null;

    // ---------- Chessboard config ----------
    var config = {
      draggable: true,
      position: 'start',
      onDragStart: onDragStart,
      onDrop: onDrop,
      onMouseoutSquare: onMouseoutSquare,
      onMouseoverSquare: onMouseoverSquare,
      onSnapEnd: onSnapEnd,
    };
    board = Chessboard('myBoard', config);

    // ---------- PST / material (as in your base) ----------
    var weights = { p: 100, n: 280, b: 320, r: 479, q: 929, k: 60000, k_e: 60000 };
    var pst_w = {
      p: [[100,100,100,100,105,100,100,100],[78,83,86,73,102,82,85,90],[7,29,21,44,40,31,44,7],[-17,16,-2,15,14,0,15,-13],[-26,3,10,9,6,1,0,-23],[-22,9,5,-11,-10,-2,3,-19],[-31,8,-7,-37,-36,-14,3,-31],[0,0,0,0,0,0,0,0]],
      n: [[-66,-53,-75,-75,-10,-55,-58,-70],[-3,-6,100,-36,4,62,-4,-14],[10,67,1,74,73,27,62,-2],[24,24,45,37,33,41,25,17],[-1,5,31,21,22,35,2,0],[-18,10,13,22,18,15,11,-14],[-23,-15,2,0,2,0,-23,-20],[-74,-23,-26,-24,-19,-35,-22,-69]],
      b: [[-59,-78,-82,-76,-23,-107,-37,-50],[-11,20,35,-42,-39,31,2,-22],[-9,39,-32,41,52,-10,28,-14],[25,17,20,34,26,25,15,10],[13,10,17,23,17,16,0,7],[14,25,24,15,8,25,20,15],[19,20,11,6,7,6,20,16],[-7,2,-15,-12,-14,-15,-10,-10]],
      r: [[35,29,33,4,37,33,56,50],[55,29,56,67,55,62,34,60],[19,35,28,33,45,27,25,15],[0,5,16,13,18,-4,-9,-6],[-28,-35,-16,-21,-13,-29,-46,-30],[-42,-28,-42,-25,-25,-35,-26,-46],[-53,-38,-31,-26,-29,-43,-44,-53],[-30,-24,-18,5,-2,-18,-31,-32]],
      q: [[6,1,-8,-104,69,24,88,26],[14,32,60,-10,20,76,57,24],[-2,43,32,60,72,63,43,2],[1,-16,22,17,25,20,-13,-6],[-14,-15,-2,-5,-1,-10,-20,-22],[-30,-6,-13,-11,-16,-11,-16,-27],[-36,-18,0,-19,-15,-15,-21,-38],[-39,-30,-31,-13,-31,-36,-34,-42]],
      k: [[4,54,47,-99,-99,60,83,-62],[-32,10,55,56,56,55,10,3],[-62,12,-57,44,-67,28,37,-31],[-55,50,11,-4,-19,13,0,-49],[-55,-43,-52,-28,-51,-47,-8,-50],[-47,-42,-43,-79,-64,-32,-29,-32],[-4,3,-14,-50,-57,-18,13,4],[17,30,-3,-14,6,-1,40,18]],
      k_e: [[-50,-40,-30,-20,-20,-30,-40,-50],[-30,-20,-10,0,0,-10,-20,-30],[-30,-10,20,30,30,20,-10,-30],[-30,-10,30,40,40,30,-10,-30],[-30,-10,30,40,40,30,-10,-30],[-30,-10,20,30,30,20,-10,-30],[-30,-30,0,0,0,0,-30,-30],[-50,-30,-30,-30,-30,-30,-30,-50]],
    };
    var pst_b = {
      p: pst_w.p.slice().reverse(), n: pst_w.n.slice().reverse(),
      b: pst_w.b.slice().reverse(), r: pst_w.r.slice().reverse(),
      q: pst_w.q.slice().reverse(), k: pst_w.k.slice().reverse(),
      k_e: pst_w.k_e.slice().reverse(),
    };
    var pstOpponent = { w: pst_b, b: pst_w };
    var pstSelf = { w: pst_w, b: pst_b };

    // ---------- Smart evaluation helpers ----------
    const CENTER_SQUARES = new Set(['d4','e4','d5','e5']);

    function computePhase(game) {
      // returns 0..1 (1 = midgame, 0 = endgame)
      const board = game.board();
      let n=0,b=0,r=0,q=0;
      for (let r0=0;r0<8;r0++) for (let c0=0;c0<8;c0++) {
        const p = board[r0][c0];
        if (!p) continue;
        if (p.type==='n') n++;
        else if (p.type==='b') b++;
        else if (p.type==='r') r++;
        else if (p.type==='q') q++;
      }
      const maxPhase = 1*4 + 1*4 + 2*4 + 4*2; // rough
      const phaseNow = 1*n + 1*b + 2*r + 4*q;
      return Math.min(1, Math.max(0, phaseNow / maxPhase));
    }

    function coordToSquare(r,c) { return 'abcdefgh'[c] + (8 - r); }

    function countLegalMoves(game, color) {
      // chess.js gives all legal moves; just count for both sides
      return game.moves({ verbose:true }).filter(m => m.color === color).length;
    }

    function pawnStructureTerms(game) {
      const board = game.board();
      let filesW = Array(8).fill(0), filesB = Array(8).fill(0);
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = board[r][c];
        if (p && p.type==='p') {
          if (p.color==='w') filesW[c]++; else filesB[c]++;
        }
      }
      let doubledW=0,doubledB=0,isolatedW=0,isolatedB=0,passedW=0,passedB=0;
      for (let f=0;f<8;f++) {
        if (filesW[f]>1) doubledW += filesW[f]-1;
        if (filesB[f]>1) doubledB += filesB[f]-1;
        const w = filesW[f]>0, lW=(f>0 && filesW[f-1]>0), rW=(f<7 && filesW[f+1]>0);
        const b = filesB[f]>0, lB=(f>0 && filesB[f-1]>0), rB=(f<7 && filesB[f+1]>0);
        if (w && !lW && !rW) isolatedW++;
        if (b && !lB && !rB) isolatedB++;
      }
      function isPassed(r,c,color) {
        const board = game.board();
        const dir = (color==='w' ? -1 : +1);
        for (let rr=r+dir; (dir===-1 ? rr>=0 : rr<8); rr+=dir) {
          for (let cc=Math.max(0,c-1); cc<=Math.min(7,c+1); cc++) {
            const q = board[rr][cc];
            if (q && q.type==='p' && q.color!==color) return false;
          }
        }
        return true;
      }
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = game.board()[r][c];
        if (p && p.type==='p') {
          if (isPassed(r,c,p.color)) {
            if (p.color==='w') passedW++; else passedB++;
          }
        }
      }
      return { doubledW, doubledB, isolatedW, isolatedB, passedW, passedB };
    }

    function isFileOpen(board, fileIdx) {
      for (let r=0;r<8;r++) {
        const p = board[r][fileIdx];
        if (p && p.type==='p') return false;
      }
      return true;
    }
    function isFileSemiOpen(board, fileIdx, color) {
      let own=false, opp=false;
      for (let r=0;r<8;r++) {
        const p = board[r][fileIdx];
        if (!p || p.type!=='p') continue;
        if (p.color===color) own=true; else opp=true;
      }
      return (!own && opp);
    }
    function rookFileBonuses(game) {
      const b = game.board();
      let wOpen=0,wSemi=0,bOpen=0,bSemi=0;
      for (let f=0;f<8;f++) {
        const open = isFileOpen(b,f);
        const semiW = isFileSemiOpen(b,f,'w');
        const semiB = isFileSemiOpen(b,f,'b');
        let wR=0,bR=0;
        for (let r=0;r<8;r++) {
          const p = b[r][f];
          if (p && p.type==='r') { if (p.color==='w') wR++; else bR++; }
        }
        if (open) { wOpen += wR; bOpen += bR; }
        if (semiW) wSemi += wR;
        if (semiB) bSemi += bR;
      }
      return { wOpen, bOpen, wSemi, bSemi };
    }
    function kingShield(game) {
      const b = game.board();
      let kW=null, kB=null;
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = b[r][c];
        if (p && p.type==='k') {
          if (p.color==='w') kW=[r,c]; else kB=[r,c];
        }
      }
      function shield(rk,ck,color) {
        if (!rk) return 0;
        const rows = color==='w' ? [rk+1, rk+2] : [rk-1, rk-2];
        let cnt=0;
        for (let rr of rows) {
          if (rr<0||rr>7) continue;
          for (let cc=Math.max(0,ck-1); cc<=Math.min(7,ck+1); cc++) {
            const p = b[rr][cc];
            if (p && p.type==='p' && p.color===color) cnt++;
          }
        }
        return Math.min(cnt,3);
      }
      return { w: shield(kW?.[0],kW?.[1],'w'), b: shield(kB?.[0],kB?.[1],'b') };
    }
    function controlCenter(game) {
      const b = game.board();
      let w=0,bk=0;
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = b[r][c]; if (!p) continue;
        const sq = coordToSquare(r,c);
        if (!CENTER_SQUARES.has(sq)) continue;
        if (p.color==='w') w++; else bk++;
      }
      return { w, b: bk };
    }

    // Full smart evaluation (from Black's pov)
    function evaluatePosition(game) {
      // terminal draws / mates first
      if (game.in_checkmate()) return (game.turn()==='b') ? -1e10 : 1e10; // if it's black to move & mate -> bad
      if (game.in_draw() || game.in_threefold_repetition() || game.in_stalemate()) return 0;

      const boardArr = game.board();

      // Material + PST
      let base = 0;
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = boardArr[r][c]; if (!p) continue;
        const t = p.type, col = p.color;
        const val = weights[t];
        const table = (col === 'w') ? pst_w : pst_b;
        base += (col === 'b' ? +val : -val);
        base += (col === 'b' ? table[t][r][c] : -table[t][r][c]);
      }

      const phase = computePhase(game); // 1 midgame -> 0 endgame
      const { doubledW, doubledB, isolatedW, isolatedB, passedW, passedB } = pawnStructureTerms(game);
      const ks = kingShield(game);
      const cc = controlCenter(game);
      const rf = rookFileBonuses(game);
      const mobW = countLegalMoves(game, 'w');
      const mobB = countLegalMoves(game, 'b');

      // bishop pair
      let bishopsW=0,bishopsB=0;
      for (let r=0;r<8;r++) for (let c=0;c<8;c++) {
        const p = boardArr[r][c];
        if (p && p.type==='b') { if (p.color==='w') bishopsW++; else bishopsB++; }
      }
      const bishopPairW = (bishopsW>=2)?1:0;
      const bishopPairB = (bishopsB>=2)?1:0;

      const MG = { mob:2, iso:-12, dbl:-8, pass:18, bp:30, ksh:8, center:6, rookOpen:15, rookSemi:8 };
      const EG = { mob:1, iso:-10, dbl:-8, pass:28, bp:20, ksh:4, center:4, rookOpen:12, rookSemi:6 };
      function blend(mg, eg) { return Math.round(mg*phase + eg*(1-phase)); }

      let posW=0,posB=0;
      posW += blend(MG.mob,EG.mob) * mobW; posB += blend(MG.mob,EG.mob) * mobB;
      posW += blend(MG.iso,EG.iso) * isolatedW; posB += blend(MG.iso,EG.iso) * isolatedB;
      posW += blend(MG.dbl,EG.dbl) * doubledW;  posB += blend(MG.dbl,EG.dbl) * doubledB;
      posW += blend(MG.pass,EG.pass)* passedW;  posB += blend(MG.pass,EG.pass)* passedB;
      posW += blend(MG.bp,EG.bp)   * bishopPairW; posB += blend(MG.bp,EG.bp)   * bishopPairB;
      posW += blend(MG.ksh,EG.ksh) * ks.w;        posB += blend(MG.ksh,EG.ksh) * ks.b;
      posW += blend(MG.center,EG.center) * cc.w;  posB += blend(MG.center,EG.center) * cc.b;
      posW += blend(MG.rookOpen,EG.rookOpen) * rf.wOpen; posB += blend(MG.rookOpen,EG.rookOpen) * rf.bOpen;
      posW += blend(MG.rookSemi,EG.rookSemi) * rf.wSemi; posB += blend(MG.rookSemi,EG.rookSemi) * rf.bSemi;

      const positional = (posB - posW);
      return base + positional;
    }

    // keep your fast incremental eval for updating globalSum (used in UI and incremental sums in search)
    function evaluateBoardIncremental(game, move, prevSum, color) {
      if (game.in_checkmate()) return (move.color === color) ? 1e10 : -1e10;
      if (game.in_draw() || game.in_threefold_repetition() || game.in_stalemate()) return 0;

      if (game.in_check()) prevSum += (move.color === color ? 50 : -50);

      var from = [8 - parseInt(move.from[1], 10), move.from.charCodeAt(0) - 97];
      var to   = [8 - parseInt(move.to[1], 10),   move.to.charCodeAt(0) - 97];

      if (prevSum < -1500 && move.piece === 'k') move.piece = 'k_e';

      if ('captured' in move) {
        if (move.color === color) {
          prevSum += weights[move.captured] + pstOpponent[move.color][move.captured][to[0]][to[1]];
        } else {
          prevSum -= weights[move.captured] + pstSelf[move.color][move.captured][to[0]][to[1]];
        }
      }

      if (typeof move.flags === 'string' && move.flags.indexOf('p') !== -1) {
        move.promotion = 'q';
        if (move.color === color) {
          prevSum -= weights[move.piece] + pstSelf[move.color][move.piece][from[0]][from[1]];
          prevSum += weights[move.promotion] + pstSelf[move.color][move.promotion][to[0]][to[1]];
        } else {
          prevSum += weights[move.piece] + pstSelf[move.color][move.piece][from[0]][from[1]];
          prevSum -= weights[move.promotion] + pstSelf[move.color][move.promotion][to[0]][to[1]];
        }
      } else {
        if (move.color !== color) {
          prevSum += pstSelf[move.color][move.piece][from[0]][from[1]];
          prevSum -= pstSelf[move.color][move.piece][to[0]][to[1]];
        } else {
          prevSum -= pstSelf[move.color][move.piece][from[0]][from[1]];
          prevSum += pstSelf[move.color][move.piece][to[0]][to[1]];
        }
      }
      return prevSum;
    }

    // ---------- Transposition table ----------
    var TT = new Map();
    function ttKey(fen, depth, isMax) { return fen + '|' + depth + '|' + (isMax ? 'M' : 'm'); }

    // ---------- Move ordering helpers ----------
    function isPromotionMove(m) {
      // ugly move: has m.promotion (piece letter)
      // pretty move: flags include 'p'
      if (m && typeof m === 'object') {
        if (m.promotion) return true;
        if (typeof m.flags === 'string' && m.flags.indexOf('p') !== -1) return true;
      }
      return false;
    }
    function pieceValForMVVLVA(ch) {
      // mvv-lva needs attacker & victim
      return (ch==='p')?100:(ch==='n')?300:(ch==='b')?320:(ch==='r')?500:(ch==='q')?900:10000;
    }
    function mvvLvaScore(m) {
      // works on ugly move (before make): m.captured (letter), m.piece (attacker)
      if (m.captured) {
        const victim = pieceValForMVVLVA(m.captured);
        const attacker = pieceValForMVVLVA(m.piece);
        return 10000 + (victim*10 - attacker); // big base to outrank quiets
      }
      if (isPromotionMove(m)) return 9000;
      return 0;
    }
    function orderMoves(moves, hashMove, ply) {
      // put hash move first, then MVV-LVA captures/promotions, then others
      return moves.slice().sort(function(a,b){
        if (hashMove) {
          const aHash = (a.from===hashMove.from && a.to===hashMove.to && a.promotion===hashMove.promotion);
          const bHash = (b.from===hashMove.from && b.to===hashMove.to && b.promotion===hashMove.promotion);
          if (aHash && !bHash) return -1;
          if (!aHash && bHash) return 1;
        }
        const sa = mvvLvaScore(a);
        const sb = mvvLvaScore(b);
        return sb - sa; // desc
      });
    }

    // ---------- Quiescence (captures only) ----------
    function quiescence(alpha, beta, standSum, color, isMax, qDepth) {
      // use full eval for stand-pat to stabilize horizon
      var standPat = evaluatePosition(game);

      if (isMax) {
        if (standPat >= beta) return beta;
        if (standPat > alpha) alpha = standPat;
      } else {
        if (standPat <= alpha) return alpha;
        if (standPat < beta) beta = standPat;
      }

      if (qDepth <= 0) return standPat;

      // explore tactical captures / promotions only
      var caps = game.ugly_moves({ verbose:true }).filter(function(m){
        return !!m.captured || !!m.promotion;
      });
      caps = orderMoves(caps, null, 0);

      for (var i=0;i<caps.length;i++) {
        var mv = caps[i];
        var pretty = game.ugly_move(mv);
        var newSum = evaluateBoardIncremental(game, pretty, standSum, color);
        var score = quiescence(alpha, beta, newSum, color, !isMax, qDepth-1);
        game.undo();

        if (isMax) {
          if (score > alpha) alpha = score;
        } else {
          if (score < beta) beta = score;
        }
        if (alpha >= beta) break;
      }
      return isMax ? alpha : beta;
    }

    // ---------- Alpha-beta with TT + ordering ----------
    function search(depth, alpha, beta, isMax, sum, color) {
      positionCount++;
      var fen = game.fen();
      var key = ttKey(fen, depth, isMax);
      var ttHit = TT.get(key);
      var ttBest = ttHit ? ttHit.best : null;

      // legal moves (ugly)
      var moves = game.ugly_moves({ verbose:true });

      // terminal or leaf
      if (depth === 0 || moves.length === 0) {
        // full evaluation at leaf, then quiescence around it
        var leafEval = evaluatePosition(game);
        var q = quiescence(alpha, beta, leafEval, color, isMax, 6);
        TT.set(key, { depth: 0, value: q, flag: 'EXACT', best: null });
        return [null, q];
      }

      // move ordering
      moves = orderMoves(moves, ttBest, 0);

      var bestMove = null;
      var value = isMax ? -Infinity : +Infinity;

      for (var i=0;i<moves.length;i++) {
        var m = moves[i];
        var pretty = game.ugly_move(m);

        // incremental running eval (fast) for tree propagation
        var newSum = evaluateBoardIncremental(game, pretty, sum, color);

        var child = search(depth-1, alpha, beta, !isMax, newSum, color);
        var childVal = child[1];

        game.undo();

        if (isMax) {
          if (childVal > value) { value = childVal; bestMove = pretty; }
          if (childVal > alpha) alpha = childVal;
        } else {
          if (childVal < value) { value = childVal; bestMove = pretty; }
          if (childVal < beta) beta = childVal;
        }
        if (alpha >= beta) break;
      }

      TT.set(key, { depth: depth, value: value, flag: 'EXACT', best: bestMove ? {from:bestMove.from, to:bestMove.to, promotion:bestMove.promotion} : null });
      return [bestMove, value];
    }

    function safeDepthFor(color) {
      var sel = (color === 'b') ? $('#search-depth') : $('#search-depth-white');
      if (sel.length === 0) return 3;
      var txt = sel.find(':selected').text();
      var d = parseInt(txt, 10);
      if (!Number.isFinite(d) || d <= 0) {
        console.warn('[AI] invalid depth for', color, '→ fallback to 3. raw=', txt);
        return 3;
      }
      return d;
    }

    function getBestMove(game, color, currSum) {
      positionCount = 0;
      var depth = safeDepthFor(color);

      var t0 = Date.now();
      var res = search(depth, -Infinity, +Infinity, true, currSum, color);
      var t1 = Date.now();

      var moveTime = t1 - t0;
      var pps = (positionCount * 1000) / Math.max(1, moveTime);
      $('#position-count').text(positionCount);
      $('#time').text(Math.round(moveTime) / 1000);
      $('#positions-per-s').text(Math.round(pps));

      return res; // [bestMove, score]
    }

    function makeBestMove(color) {
      var tuple = (color === 'b')
        ? getBestMove(game, color, globalSum)
        : getBestMove(game, color, -globalSum);

      var move = tuple[0];
      if (!move) return;

      // update fast running eval for UI bar (from Black's pov)
      globalSum = evaluateBoardIncremental(game, move, globalSum, 'b');
      updateAdvantage();

      game.move(move);
      board.position(game.fen());

      if (color === 'b') {
        checkStatus('black');
        $board.find('.' + squareClass).removeClass('highlight-black');
        $board.find('.square-' + move.from).addClass('highlight-black');
        squareToHighlight = move.to; colorToHighlight = 'black';
        $board.find('.square-' + squareToHighlight).addClass('highlight-' + colorToHighlight);
      } else {
        checkStatus('white');
        $board.find('.' + squareClass).removeClass('highlight-white');
        $board.find('.square-' + move.from).addClass('highlight-white');
        squareToHighlight = move.to; colorToHighlight = 'white';
        $board.find('.square-' + squareToHighlight).addClass('highlight-' + colorToHighlight);
      }
    }

    // ---------- UI helpers / unchanged ----------
    function checkStatus(color) {
      if (game.in_checkmate()) {
        $('#status').html(`<b>Checkmate!</b> Oops, <b>${color}</b> lost.`);
      } else if (game.insufficient_material()) {
        $('#status').html(`It's a <b>draw!</b> (Insufficient Material)`);
      } else if (game.in_threefold_repetition()) {
        $('#status').html(`It's a <b>draw!</b> (Threefold Repetition)`);
      } else if (game.in_stalemate()) {
        $('#status').html(`It's a <b>draw!</b> (Stalemate)`);
      } else if (game.in_draw()) {
        $('#status').html(`It's a <b>draw!</b> (50-move Rule)`);
      } else if (game.in_check()) {
        $('#status').html(`Oops, <b>${color}</b> is in <b>check!</b>`);
        return false;
      } else {
        $('#status').html(`No check, checkmate, or draw.`);
        return false;
      }
      return true;
    }

    function updateAdvantage() {
      if (globalSum > 0) { $('#advantageColor').text('Black'); $('#advantageNumber').text(globalSum);
      } else if (globalSum < 0) { $('#advantageColor').text('White'); $('#advantageNumber').text(-globalSum);
      } else { $('#advantageColor').text('Neither side'); $('#advantageNumber').text(globalSum); }
      $('#advantageBar').attr({ 'aria-valuenow': `${-globalSum}`, style: `width: ${((-globalSum + 2000) / 4000) * 100}%` });
    }

    // ------ Buttons ------
    $('#ruyLopezBtn').on('click', function () {
      reset();
      game.load('r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 1');
      board.position(game.fen());
      window.setTimeout(function () { makeBestMove('b'); }, 250);
    });
    $('#italianGameBtn').on('click', function () {
      reset();
      game.load('r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 1');
      board.position(game.fen());
      window.setTimeout(function () { makeBestMove('b'); }, 250);
    });
    $('#sicilianDefenseBtn').on('click', function () {
      reset();
      game.load('rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1');
      board.position(game.fen());
    });
    $('#startBtn').on('click', reset);

    $('#compVsCompBtn').on('click', function () {
      reset();
      compVsComp('w');
    });
    $('#resetBtn').on('click', reset);

    var undo_stack = [];
    function undo() {
      var move = game.undo();
      undo_stack.push(move);
      if (undo_stack.length > STACK_SIZE) undo_stack.shift();
      board.position(game.fen());
    }
    $('#undoBtn').on('click', function () {
      if (game.history().length >= 2) {
        $board.find('.' + squareClass).removeClass('highlight-white highlight-black highlight-hint');
        undo();
        window.setTimeout(function () {
          undo();
          window.setTimeout(function () { showHint(); }, 250);
        }, 250);
      } else {
        alert('Nothing to undo.');
      }
    });

    function redo() {
      game.move(undo_stack.pop());
      board.position(game.fen());
    }
    $('#redoBtn').on('click', function () {
      if (undo_stack.length >= 2) {
        redo();
        window.setTimeout(function () {
          redo();
          window.setTimeout(function () { showHint(); }, 250);
        }, 250);
      } else {
        alert('Nothing to redo.');
      }
    });

    $('#showHint').change(function () { window.setTimeout(showHint, 250); });
    function showHint() {
      var chk = document.getElementById('showHint');
      $board.find('.' + squareClass).removeClass('highlight-hint');
      if (!chk || !chk.checked) return;
      var move = getBestMove(game, 'w', -globalSum)[0];
      if (!move) return;
      $board.find('.square-' + move.from).addClass('highlight-hint');
      $board.find('.square-' + move.to).addClass('highlight-hint');
    }

    // ------ chessboard.js helpers ------
    function removeGreySquares() { $('#myBoard .square-55d63').css('background', ''); }
    function greySquare(square) {
      var $square = $('#myBoard .square-' + square);
      var background = $square.hasClass('black-3c85d') ? blackSquareGrey : whiteSquareGrey;
      $square.css('background', background);
    }

    function onDragStart(source, piece) {
      if (game.game_over()) return false;
      if ((game.turn() === 'w' && /^b/.test(piece)) || (game.turn() === 'b' && /^w/.test(piece))) return false;
    }

    function onDrop(source, target) {
      undo_stack = [];
      removeGreySquares();

      var move = game.move({ from: source, to: target, promotion: 'q' });
      if (move === null) return 'snapback';

      // update UI eval
      globalSum = evaluateBoardIncremental(game, move, globalSum, 'b');
      updateAdvantage();

      $board.find('.' + squareClass).removeClass('highlight-white');
      $board.find('.square-' + move.from).addClass('highlight-white');
      squareToHighlight = move.to; colorToHighlight = 'white';
      $board.find('.square-' + squareToHighlight).addClass('highlight-' + colorToHighlight);

      // (fixed: no stray semicolon)
      if (!checkStatus('black')) { 
        window.setTimeout(function () {
          makeBestMove('b');
          window.setTimeout(function () { showHint(); }, 250);
        }, 250);
      }
    }

    function onMouseoverSquare(square, piece) {
      var moves = game.moves({ square: square, verbose: true });
      if (moves.length === 0) return;
      greySquare(square);
      for (var i = 0; i < moves.length; i++) greySquare(moves[i].to);
    }

    function onMouseoutSquare(square, piece) { removeGreySquares(); }
    function onSnapEnd() { board.position(game.fen()); }

    function compVsComp(color) {
      if (!checkStatus({ w: 'white', b: 'black' }[color])) {
        timer = window.setTimeout(function () {
          makeBestMove(color);
          compVsComp(color === 'w' ? 'b' : 'w');
        }, 250);
      }
    }

    function reset() {
      game.reset();
      globalSum = 0;
      $board.find('.' + squareClass).removeClass('highlight-white highlight-black highlight-hint');
      board.position(game.fen());
      $('#advantageColor').text('Neither side');
      $('#advantageNumber').text(globalSum);
      if (timer) { clearTimeout(timer); timer = null; }
      TT.clear();
    }
  }
})();
