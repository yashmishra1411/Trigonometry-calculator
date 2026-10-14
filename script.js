(function () {
  'use strict';
  var D2R = Math.PI / 180, R2D = 180 / Math.PI;

  /* ---------- number helpers ---------- */
  function clean(v) {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    var r = Math.round(v);
    if (Math.abs(v - r) < 1e-10) return r === 0 ? 0 : r;
    return v;
  }
  function fmt(v, dp) {
    if (v === null || v === undefined || typeof v !== 'number' || isNaN(v)) return 'undefined';
    if (!isFinite(v)) return v > 0 ? '∞' : '-∞';
    v = clean(v);
    if (Number.isInteger(v)) return String(v);
    if (Math.abs(v) >= 1e9 || Math.abs(v) < 1e-5) return v.toExponential(4);
    return String(parseFloat(v.toFixed(dp == null ? 5 : dp)));
  }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; }); }

  /* ---------- expression parser ---------- */
  var FUNCS = ['sin', 'cos', 'tan', 'sec', 'csc', 'cot', 'asin', 'acos', 'atan', 'sqrt', 'abs', 'ln', 'log', 'exp'];
  var BYLEN = FUNCS.slice().sort(function (a, b) { return b.length - a.length; });

  function normalize(s) {
    return String(s).toLowerCase()
      .replace(/π/g, 'pi').replace(/×/g, '*').replace(/÷/g, '/').replace(/[−–]/g, '-')
      .replace(/√/g, 'sqrt').replace(/²/g, '^2').replace(/³/g, '^3')
      .replace(/\bdeg(rees?)?\b/g, '°').replace(/\barc(sin|cos|tan)/g, 'a$1')
      .replace(/\s+/g, ' ').trim();
  }
  function tokenize(s) {
    var t = [], i = 0, m;
    function word(w) {
      if (FUNCS.indexOf(w) >= 0 || w === 'pi' || w === 'e' || w === 'x') { t.push({ t: 'id', v: w }); return; }
      for (var k = 0; k < BYLEN.length; k++) {
        var f = BYLEN[k];
        if (w.indexOf(f) === 0 && w.length > f.length) { t.push({ t: 'id', v: f }); word(w.slice(f.length)); return; }
      }
      if (w.indexOf('pi') === 0 && w.length > 2) { t.push({ t: 'id', v: 'pi' }); word(w.slice(2)); return; }
      throw new Error('I don\'t recognise "' + w + '".');
    }
    while (i < s.length) {
      var ch = s.charAt(i);
      if (ch === ' ') { i++; continue; }
      m = /^(\d+\.?\d*|\.\d+)/.exec(s.slice(i));
      if (m) { t.push({ t: 'num', v: parseFloat(m[1]) }); i += m[1].length; continue; }
      m = /^[a-z]+/.exec(s.slice(i));
      if (m) { word(m[0]); i += m[0].length; continue; }
      if ('+-*/^(),°'.indexOf(ch) >= 0) { t.push({ t: ch }); i++; continue; }
      throw new Error('I can\'t read the symbol "' + ch + '".');
    }
    return t;
  }
  function applyFn(name, v, m) {
    var toRad = m === 'deg' ? D2R : 1, fromRad = m === 'deg' ? R2D : 1, r, s, c;
    switch (name) {
      case 'sin': return Math.sin(v * toRad);
      case 'cos': return Math.cos(v * toRad);
      case 'tan': r = v * toRad; c = Math.cos(r); return Math.abs(c) < 1e-12 ? NaN : Math.sin(r) / c;
      case 'sec': c = Math.cos(v * toRad); return Math.abs(c) < 1e-12 ? NaN : 1 / c;
      case 'csc': s = Math.sin(v * toRad); return Math.abs(s) < 1e-12 ? NaN : 1 / s;
      case 'cot': r = v * toRad; s = Math.sin(r); return Math.abs(s) < 1e-12 ? NaN : Math.cos(r) / s;
      case 'asin': return Math.abs(v) > 1 ? NaN : Math.asin(v) * fromRad;
      case 'acos': return Math.abs(v) > 1 ? NaN : Math.acos(v) * fromRad;
      case 'atan': return Math.atan(v) * fromRad;
      case 'sqrt': return v < 0 ? NaN : Math.sqrt(v);
      case 'abs': return Math.abs(v);
      case 'ln': return v > 0 ? Math.log(v) : NaN;
      case 'log': return v > 0 ? Math.log(v) / Math.LN10 : NaN;
      case 'exp': return Math.exp(v);
    }
    return NaN;
  }
  function parse(src, m) {
    var tk = tokenize(normalize(src)), p = 0;
    if (!tk.length) throw new Error('Type an expression first.');
    function peek() { return tk[p]; }
    function eat(t) { if (tk[p] && tk[p].t === t) { return tk[p++]; } return null; }
    function startsFactor(t) { return t && (t.t === 'num' || t.t === 'id' || t.t === '('); }
    function isValueId(t) { return t && t.t === 'id' && (t.v === 'x' || t.v === 'pi' || t.v === 'e'); }
    function bin(l, r, op) {
      return function (env) {
        var a = l(env), b = r(env);
        if (op === '+') return a + b;
        if (op === '-') return a - b;
        if (op === '*') return a * b;
        return b === 0 ? NaN : a / b;
      };
    }
    function expr() {
      var l = term();
      for (;;) {
        if (eat('+')) l = bin(l, term(), '+');
        else if (eat('-')) l = bin(l, term(), '-');
        else break;
      }
      return l;
    }
    function term() {
      var l = unary();
      for (;;) {
        if (eat('*')) l = bin(l, unary(), '*');
        else if (eat('/')) l = bin(l, unary(), '/');
        else if (startsFactor(peek())) l = bin(l, power(), '*');
        else break;
      }
      return l;
    }
    function unary() {
      if (eat('-')) { var u = unary(); return function (env) { return -u(env); }; }
      if (eat('+')) return unary();
      return power();
    }
    function power() {
      var b = postfix();
      if (eat('^')) {
        var e = unary();
        return function (env) { var x = b(env), y = e(env), r = Math.pow(x, y); return isFinite(r) ? r : NaN; };
      }
      return b;
    }
    function postfix() {
      var n = primary();
      while (eat('°')) {
        (function (inner) { n = function (env) { return m === 'rad' ? inner(env) * D2R : inner(env); }; })(n);
      }
      return n;
    }
    function funcArg() {
      var n = power();
      while (isValueId(peek())) n = bin(n, power(), '*');
      return n;
    }
    function primary() {
      var t = tk[p++];
      if (!t) throw new Error('The expression ends too soon. Check for a missing number or bracket.');
      if (t.t === 'num') { var v = t.v; return function () { return v; }; }
      if (t.t === '(') {
        var e = expr();
        if (!eat(')')) throw new Error('A bracket is not closed.');
        return e;
      }
      if (t.t === 'id') {
        if (t.v === 'pi') return function () { return Math.PI; };
        if (t.v === 'e') return function () { return Math.E; };
        if (t.v === 'x') return function (env) {
          if (!env || env.x === undefined) throw new Error('x has no value here. Use the Identities tab for expressions with x.');
          return env.x;
        };
        var name = t.v, arg;
        if (peek() && peek().t === '(') {
          eat('(');
          arg = expr();
          if (!eat(')')) throw new Error('A bracket is not closed.');
        } else {
          if (!startsFactor(peek()) && !(peek() && peek().t === '-')) throw new Error(name + ' needs an angle or number after it.');
          arg = peek().t === '-' ? (eat('-'), (function (a) { return function (env) { return -a(env); }; })(funcArg())) : funcArg();
        }
        return function (env) { return applyFn(name, arg(env), m); };
      }
      throw new Error('Unexpected "' + t.t + '".');
    }
    var f = expr();
    if (p < tk.length) throw new Error('I got stuck at "' + (tk[p].v !== undefined ? tk[p].v : tk[p].t) + '". Check the brackets and operators.');
    return f;
  }
  function evalNumber(str) {
    var s = String(str).trim();
    if (s === '') return null;
    var v = parse(s, 'deg')({});
    if (typeof v !== 'number' || !isFinite(v)) throw new Error('"' + s + '" is not a usable number.');
    return v;
  }

  /* ---------- exact values ---------- */
  var BASE = {
    sin: { 0: '0', 30: '1/2', 45: '√2/2', 60: '√3/2', 90: '1' },
    cos: { 0: '1', 30: '√3/2', 45: '√2/2', 60: '1/2', 90: '0' },
    tan: { 0: '0', 30: '√3/3', 45: '1', 60: '√3', 90: null }
  };
  var RECIP = { '1': '1', '1/2': '2', '√2/2': '√2', '√3/2': '2√3/3', '√3/3': '√3', '√3': '√3/3' };
  function exactValue(fn, deg) {
    var a = ((deg % 360) + 360) % 360;
    if (!(a % 30 === 0 || a % 45 === 0)) return null;
    var ref = a <= 90 ? a : a <= 180 ? 180 - a : a <= 270 ? a - 180 : 360 - a;
    var quad = a === 0 || a === 90 || a === 180 || a === 270 ? 0 : a < 90 ? 1 : a < 180 ? 2 : a < 270 ? 3 : 4;
    function signed(name) {
      var base = BASE[name][ref];
      if (base === null) return { v: null };
      if (base === '0') return { v: '0' };
      var neg;
      if (name === 'sin') neg = (a > 180);
      else if (name === 'cos') neg = (a > 90 && a < 270);
      else neg = (a > 90 && a < 180) || (a > 270);
      return { v: (neg ? '-' : '') + base };
    }
    var res, base2;
    if (fn === 'sin' || fn === 'cos' || fn === 'tan') {
      res = signed(fn);
    } else {
      base2 = signed(fn === 'sec' ? 'cos' : fn === 'csc' ? 'sin' : 'tan');
      if (fn === 'cot') {
        if (base2.v === null) res = { v: '0' };
        else if (base2.v === '0') res = { v: null };
        else res = { v: (base2.v.charAt(0) === '-' ? '-' : '') + RECIP[base2.v.replace('-', '')] };
      } else {
        if (base2.v === null || base2.v === '0') res = { v: null };
        else res = { v: (base2.v.charAt(0) === '-' ? '-' : '') + RECIP[base2.v.replace('-', '')] };
      }
    }
    return { value: res.v, ref: ref, quad: quad, angle: a };
  }
  var QUAD = ['on an axis', 'Quadrant I', 'Quadrant II', 'Quadrant III', 'Quadrant IV'];

  /* ---------- triangles ---------- */
  function sinD(d) { return Math.sin(d * D2R); }
  function cosD(d) { return Math.cos(d * D2R); }
  function acosD(v) { return Math.acos(Math.max(-1, Math.min(1, v))) * R2D; }
  function asinD(v) { return Math.asin(Math.max(-1, Math.min(1, v))) * R2D; }
  function d3(v) { return fmt(v, 3); }
  function pos(v, name) { if (v !== null && !(v > 0)) throw new Error(name + ' must be greater than 0.'); }

  function solveRight(i) {
    var a = i.a, b = i.b, c = i.c, A = i.A, B = i.B, st = [];
    var known = [a, b, c, A, B].filter(function (v) { return v !== null; }).length;
    if (known !== 2) throw new Error('Enter exactly two values for a right triangle.');
    if (a === null && b === null && c === null) throw new Error('At least one side is needed. Angles alone only give the shape.');
    pos(a, 'Side a'); pos(b, 'Side b'); pos(c, 'Side c');
    [['A', A], ['B', B]].forEach(function (p) { if (p[1] !== null && !(p[1] > 0 && p[1] < 90)) throw new Error('Angle ' + p[0] + ' must be between 0° and 90° in a right triangle.'); });
    if (A === null && B !== null) { A = 90 - B; st.push('Angles in a triangle add to 180°, and C = 90°, so A = 90° − B = ' + d3(A) + '°.'); }
    else if (B === null && A !== null) { B = 90 - A; st.push('Angles in a triangle add to 180°, and C = 90°, so B = 90° − A = ' + d3(B) + '°.'); }
    if (A !== null) {
      if (a !== null) {
        c = a / sinD(A); b = a / Math.tan(A * D2R);
        st.push('sin A = a / c, so c = a / sin A = ' + d3(a) + ' / sin ' + d3(A) + '° = ' + d3(c) + '.');
        st.push('tan A = a / b, so b = a / tan A = ' + d3(b) + '.');
      } else if (b !== null) {
        c = b / cosD(A); a = b * Math.tan(A * D2R);
        st.push('cos A = b / c, so c = b / cos A = ' + d3(b) + ' / cos ' + d3(A) + '° = ' + d3(c) + '.');
        st.push('tan A = a / b, so a = b · tan A = ' + d3(a) + '.');
      } else {
        a = c * sinD(A); b = c * cosD(A);
        st.push('sin A = a / c, so a = c · sin A = ' + d3(c) + ' · sin ' + d3(A) + '° = ' + d3(a) + '.');
        st.push('cos A = b / c, so b = c · cos A = ' + d3(b) + '.');
      }
    } else {
      if (a !== null && b !== null) { c = Math.sqrt(a * a + b * b); st.push('Pythagoras: c² = a² + b², so c = √(' + d3(a * a) + ' + ' + d3(b * b) + ') = ' + d3(c) + '.'); }
      else if (a !== null) {
        if (!(c > a)) throw new Error('The hypotenuse c must be longer than side a.');
        b = Math.sqrt(c * c - a * a); st.push('Pythagoras: b² = c² − a², so b = √(' + d3(c * c) + ' − ' + d3(a * a) + ') = ' + d3(b) + '.');
      } else {
        if (!(c > b)) throw new Error('The hypotenuse c must be longer than side b.');
        a = Math.sqrt(c * c - b * b); st.push('Pythagoras: a² = c² − b², so a = √(' + d3(c * c) + ' − ' + d3(b * b) + ') = ' + d3(a) + '.');
      }
      A = Math.atan(a / b) * R2D;
      st.push('tan A = a / b, so A = tan⁻¹(' + d3(a) + ' / ' + d3(b) + ') = ' + d3(A) + '°.');
      B = 90 - A;
      st.push('B = 90° − A = ' + d3(B) + '°.');
    }
    return [{ s: [a, b, c], a: [A, B, 90], steps: st, method: 'Right-triangle ratios' }];
  }

  function solveGeneral(S, A) {
    var L = ['a', 'b', 'c'], V = ['A', 'B', 'C'];
    var nS = S.filter(function (v) { return v !== null; }).length, nA = A.filter(function (v) { return v !== null; }).length;
    if (nS + nA !== 3) throw new Error('Enter exactly three values.');
    if (nS === 0) throw new Error('At least one side is needed. Three angles only give the shape, not the size.');
    S.forEach(function (v, i) { pos(v, 'Side ' + L[i]); });
    A.forEach(function (v, i) { if (v !== null && !(v > 0 && v < 180)) throw new Error('Angle ' + V[i] + ' must be between 0° and 180°.'); });
    var i, j, k, st = [];

    if (nS === 3) {
      if (!(S[0] + S[1] > S[2] && S[0] + S[2] > S[1] && S[1] + S[2] > S[0])) throw new Error('These sides can\'t form a triangle. The two shorter sides must add up to more than the longest.');
      var ang = [];
      for (i = 0; i < 2; i++) {
        j = (i + 1) % 3; k = (i + 2) % 3;
        ang[i] = acosD((S[j] * S[j] + S[k] * S[k] - S[i] * S[i]) / (2 * S[j] * S[k]));
        st.push('Law of cosines: cos ' + V[i] + ' = (' + L[j] + '² + ' + L[k] + '² − ' + L[i] + '²) / (2' + L[j] + L[k] + ') so ' + V[i] + ' = ' + d3(ang[i]) + '°.');
      }
      ang[2] = 180 - ang[0] - ang[1];
      st.push('Angles add to 180°: C = 180° − A − B = ' + d3(ang[2]) + '°.');
      return [{ s: S.slice(), a: ang, steps: st, method: 'Three sides (law of cosines)' }];
    }

    if (nS === 2 && nA === 1) {
      var kA = A.map(function (v, idx) { return v !== null ? idx : -1; }).filter(function (v) { return v >= 0; })[0];
      if (S[kA] === null) {
        var o = [0, 1, 2].filter(function (v) { return v !== kA; });
        var x = o[0], y = o[1], K = A[kA];
        var u = Math.sqrt(S[x] * S[x] + S[y] * S[y] - 2 * S[x] * S[y] * cosD(K));
        var sides = S.slice(); sides[kA] = u;
        st.push('Law of cosines: ' + L[kA] + '² = ' + L[x] + '² + ' + L[y] + '² − 2·' + L[x] + '·' + L[y] + '·cos ' + V[kA] + ', so ' + L[kA] + ' = ' + d3(u) + '.');
        var angs = A.slice();
        angs[x] = acosD((S[y] * S[y] + u * u - S[x] * S[x]) / (2 * S[y] * u));
        st.push('Law of cosines again: ' + V[x] + ' = ' + d3(angs[x]) + '°.');
        angs[y] = 180 - K - angs[x];
        st.push('Angles add to 180°: ' + V[y] + ' = 180° − ' + V[kA] + ' − ' + V[x] + ' = ' + d3(angs[y]) + '°.');
        return [{ s: sides, a: angs, steps: st, method: 'Two sides and the angle between (law of cosines)' }];
      }
      var p = kA, q = [0, 1, 2].filter(function (v) { return v !== p && S[v] !== null; })[0], r = [0, 1, 2].filter(function (v) { return v !== p && v !== q; })[0];
      var Kd = A[p], sinQ = S[q] * sinD(Kd) / S[p];
      st.push('Law of sines: sin ' + V[q] + ' = ' + L[q] + ' · sin ' + V[p] + ' / ' + L[p] + ' = ' + fmt(sinQ, 5) + '.');
      if (sinQ > 1 + 1e-9) throw new Error('No triangle fits. The side opposite the given angle is too short to reach (sin ' + V[q] + ' would be ' + fmt(sinQ, 4) + ', which is more than 1).');
      var q1 = asinD(Math.min(1, sinQ)), cands = [q1];
      if (Math.abs(sinQ - 1) > 1e-9) cands.push(180 - q1);
      var sols = [];
      cands.forEach(function (Q, idx) {
        if (Kd + Q >= 180 - 1e-9) return;
        var R = 180 - Kd - Q, rs = S[p] * sinD(R) / sinD(Kd);
        var sd = S.slice(), an = A.slice();
        sd[r] = rs; an[q] = Q; an[r] = R;
        var s2 = st.slice();
        s2.push((cands.length > 1 ? 'Solution ' + (sols.length + 1) + ': ' : '') + V[q] + ' = ' + (idx === 0 ? 'sin⁻¹(' + fmt(sinQ, 5) + ')' : '180° − sin⁻¹(' + fmt(sinQ, 5) + ')') + ' = ' + d3(Q) + '°.');
        s2.push('Angles add to 180°: ' + V[r] + ' = 180° − ' + d3(Kd) + '° − ' + d3(Q) + '° = ' + d3(R) + '°.');
        s2.push('Law of sines: ' + L[r] + ' = ' + L[p] + ' · sin ' + V[r] + ' / sin ' + V[p] + ' = ' + d3(rs) + '.');
        sols.push({ s: sd, a: an, steps: s2, method: 'Two sides and a non-included angle (law of sines)', note: '' });
      });
      if (!sols.length) throw new Error('No triangle fits these values. The second angle would push the angle sum past 180°.');
      if (sols.length === 2) sols.forEach(function (s) { s.note = 'Two different triangles fit these measurements (the ambiguous case).'; });
      return sols;
    }

    var si = S.map(function (v, idx) { return v !== null ? idx : -1; }).filter(function (v) { return v >= 0; })[0];
    var an2 = A.slice(), t = A.map(function (v, idx) { return v === null ? idx : -1; }).filter(function (v) { return v >= 0; })[0];
    an2[t] = 180 - A.filter(function (v) { return v !== null; }).reduce(function (a, b) { return a + b; }, 0);
    if (!(an2[t] > 0)) throw new Error('These two angles add up to 180° or more, so no triangle fits.');
    st.push('Angles add to 180°: ' + V[t] + ' = 180° − the other two = ' + d3(an2[t]) + '°.');
    var ratio = S[si] / sinD(an2[si]), sd2 = S.slice();
    st.push('Law of sines ratio: ' + L[si] + ' / sin ' + V[si] + ' = ' + d3(S[si]) + ' / sin ' + d3(an2[si]) + '° = ' + fmt(ratio, 5) + '.');
    [0, 1, 2].forEach(function (n) {
      if (n === si) return;
      sd2[n] = ratio * sinD(an2[n]);
      st.push(L[n] + ' = ratio · sin ' + V[n] + ' = ' + fmt(ratio, 5) + ' · sin ' + d3(an2[n]) + '° = ' + d3(sd2[n]) + '.');
    });
    return [{ s: sd2, a: an2, steps: st, method: 'Two angles and a side (law of sines)' }];
  }

  function solveEquation(o) {
    var a = o.a, b = o.b, c = o.c, k = o.k, f = o.f, lo = o.lo, hi = o.hi, st = [];
    if (a === 0) throw new Error('a cannot be 0.');
    if (b === 0) throw new Error('b cannot be 0.');
    if (!(hi > lo)) throw new Error('The end of the range must be larger than the start.');
    var m = k / a;
    st.push('Divide both sides by ' + fmt(a) + ': ' + f + '(' + fmt(b) + 'x + ' + fmt(c) + '°) = ' + fmt(m, 6) + '.');
    var fams = [], period, t1;
    if (f === 'sin' || f === 'cos') {
      if (Math.abs(m) > 1 + 1e-12) return { steps: st.concat(['The value ' + fmt(m, 5) + ' is outside −1 to 1, and ' + f + ' never leaves that range. There is no solution.']), general: [], list: [], none: true };
    }
    st.push('Let θ = ' + fmt(b) + 'x + ' + fmt(c) + '°. Then ' + f + ' θ = ' + fmt(m, 6) + '.');
    var thetas = [];
    if (f === 'sin') {
      t1 = asinD(m); period = 360;
      st.push('Principal value: θ = sin⁻¹(' + fmt(m, 6) + ') = ' + d3(t1) + '°. Sine is also equal at 180° − θ.');
      thetas = [t1, 180 - t1];
      st.push('So θ = ' + d3(t1) + '° + 360°n or θ = ' + d3(180 - t1) + '° + 360°n, for whole numbers n.');
    } else if (f === 'cos') {
      t1 = acosD(m); period = 360;
      st.push('Principal value: θ = cos⁻¹(' + fmt(m, 6) + ') = ' + d3(t1) + '°. Cosine is also equal at −θ.');
      thetas = [t1, -t1];
      st.push('So θ = ' + d3(t1) + '° + 360°n or θ = ' + d3(-t1) + '° + 360°n, for whole numbers n.');
    } else {
      t1 = Math.atan(m) * R2D; period = 180;
      st.push('Principal value: θ = tan⁻¹(' + fmt(m, 6) + ') = ' + d3(t1) + '°. Tangent repeats every 180°.');
      thetas = [t1];
      st.push('So θ = ' + d3(t1) + '° + 180°n, for whole numbers n.');
    }
    var step = period / Math.abs(b);
    st.push('Solve for x using x = (θ − ' + fmt(c) + '°) / ' + fmt(b) + ', so the solutions repeat every ' + d3(step) + '°.');
    thetas.forEach(function (t) {
      var x0 = (t - c) / b, base = ((x0 % step) + step) % step;
      if (!fams.some(function (z) { return Math.abs(z - base) < 1e-7 || Math.abs(z - base - step) < 1e-7 || Math.abs(z - base + step) < 1e-7; })) fams.push(base);
    });
    var general = fams.map(function (z) { return 'x = ' + d3(z) + '° + ' + d3(step) + '°·n'; });
    var list = [];
    fams.forEach(function (base) {
      var n0 = Math.ceil((lo - base) / step - 1e-9), n1 = Math.floor((hi - base) / step + 1e-9);
      for (var n = n0; n <= n1 && n - n0 < 400; n++) {
        var v = Math.round((base + n * step) * 1e8) / 1e8;
        if (list.indexOf(v) < 0) list.push(v);
      }
    });
    list.sort(function (p, q) { return p - q; });
    return { steps: st, general: general, list: list };
  }

  var mode = 'deg';
  var $ = function (s, r) { return (r || document).querySelector(s); };
  var $$ = function (s, r) { return Array.prototype.slice.call((r || document).querySelectorAll(s)); };
  function errBox(e) { return '<div class="err" role="alert"><b>Check this:</b> ' + esc(e && e.message ? e.message : 'Something went wrong.') + '</div>'; }
  function getNum(id, label) {
    var raw = $('#' + id).value;
    try { return evalNumber(raw); } catch (e) { throw new Error(label + ': ' + e.message); }
  }

  function showTab(name) {
    $$('.tab').forEach(function (b) { b.setAttribute('aria-selected', b.getAttribute('data-tab') === name ? 'true' : 'false'); });
    $$('.panel').forEach(function (p) { p.hidden = p.id !== 'panel-' + name; });
  }
  $$('.tab').forEach(function (b) { b.addEventListener('click', function () { showTab(b.getAttribute('data-tab')); }); });

  function setMode(m) {
    mode = m;
    $$('[data-mode]').forEach(function (b) { b.setAttribute('aria-pressed', b.getAttribute('data-mode') === m ? 'true' : 'false'); });
    runExpr(); runIdentity();
  }
  $$('[data-mode]').forEach(function (b) { b.addEventListener('click', function () { setMode(b.getAttribute('data-mode')); }); });

  function runExpr() {
    var src = $('#expr').value, out = $('#out-expr');
    try {
      var f = parse(src, mode), v = f({}), unit = mode === 'deg' ? 'degrees' : 'radians';
      var html = '<div class="card"><h2>Answer</h2>';
      if (typeof v !== 'number' || isNaN(v)) {
        html += '<div class="answer"><span class="label">Result</span><span class="big small">undefined</span></div><p class="hint">That involves a division by zero or a value outside the function\'s domain, such as tan 90° or sin⁻¹ 2.</p></div>';
        out.innerHTML = html; return;
      }
      html += '<div class="answer"><span class="label">Decimal (angles in ' + unit + ')</span><span class="big">' + esc(fmt(v, 6)) + '</span></div>';
      var ex = null, mm = /^(sin|cos|tan|sec|csc|cot)\s*\(?\s*(-?\d+)\s*(°|deg|degrees)?\s*\)?$/.exec(normalize(src));
      if (mm && (mode === 'deg' || mm[3])) ex = { fn: mm[1], deg: parseInt(mm[2], 10), r: exactValue(mm[1], parseInt(mm[2], 10)) };
      if (mode === 'rad' && mm && !mm[3]) ex = null;
      if (ex && ex.r) {
        var r = ex.r;
        html += '<div class="answer"><span class="label">Exact value</span><span class="big">' + (r.value === null ? 'undefined' : esc(r.value.replace('-', '−'))) + '</span></div>';
        html += '<ol class="steps">';
        html += '<li>Reduce the angle: ' + ex.deg + '° is the same direction as ' + r.angle + '°.</li>';
        html += '<li>' + r.angle + '° is ' + (r.quad ? 'in ' + QUAD[r.quad] + ', with reference angle ' + r.ref + '°.' : 'on an axis, so use the axis value directly.') + '</li>';
        html += '<li>Look up ' + ex.fn + ' ' + r.ref + '° in the special-angle table' + (r.quad ? ' and apply the sign for ' + QUAD[r.quad] + '.' : '.') + '</li>';
        html += '</ol>';
      } else {
        html += '<p class="hint">Exact surds are shown when the expression is a single trig function of a multiple of 30° or 45°.</p>';
      }
      out.innerHTML = html + '</div>';
    } catch (e) { out.innerHTML = errBox(e); }
  }
  $('#solve-expr').addEventListener('click', runExpr);
  $$('[data-ex-expr]').forEach(function (c) {
    if (c.hidden) return;
    c.addEventListener('click', function () { $('#expr').value = c.getAttribute('data-ex-expr'); setMode(c.getAttribute('data-ex-mode')); });
  });

  function drawTri(s, an) {
    var a = s[0], b = s[1], c = s[2], A = an[0] * D2R;
    var P = [[0, 0], [c, 0], [b * Math.cos(A), -b * Math.sin(A)]];
    var xs = P.map(function (p) { return p[0]; }), ys = P.map(function (p) { return p[1]; });
    var minx = Math.min.apply(null, xs), maxx = Math.max.apply(null, xs), miny = Math.min.apply(null, ys), maxy = Math.max.apply(null, ys);
    var W = 340, H = 230, pad = 46, w = maxx - minx || 1, h = maxy - miny || 1, sc = Math.min((W - 2 * pad) / w, (H - 2 * pad) / h);
    var ox = pad + ((W - 2 * pad) - w * sc) / 2, oy = pad + ((H - 2 * pad) - h * sc) / 2;
    var T = P.map(function (p) { return [ox + (p[0] - minx) * sc, oy + (p[1] - miny) * sc]; });
    var cen = [(T[0][0] + T[1][0] + T[2][0]) / 3, (T[0][1] + T[1][1] + T[2][1]) / 3];
    function out(pt, d) { var dx = pt[0] - cen[0], dy = pt[1] - cen[1], l = Math.hypot(dx, dy) || 1; return [pt[0] + dx / l * d, pt[1] + dy / l * d]; }
    function inn(pt, f) { return [pt[0] + (cen[0] - pt[0]) * f, pt[1] + (cen[1] - pt[1]) * f]; }
    var svg = '<svg class="tri" viewBox="0 0 ' + W + ' ' + H + '" role="img" aria-label="Triangle diagram"><polygon points="' + T.map(function (p) { return p[0].toFixed(1) + ',' + p[1].toFixed(1); }).join(' ') + '"/>';
    an.forEach(function (ang, i) {
      if (Math.abs(ang - 90) < 1e-6) {
        var V = T[i], U = T[(i + 1) % 3], Z = T[(i + 2) % 3];
        function unit(from, to) { var dx = to[0] - from[0], dy = to[1] - from[1], l = Math.hypot(dx, dy) || 1; return [dx / l * 11, dy / l * 11]; }
        var u = unit(V, U), z = unit(V, Z);
        svg += '<polyline points="' + (V[0] + u[0]).toFixed(1) + ',' + (V[1] + u[1]).toFixed(1) + ' ' + (V[0] + u[0] + z[0]).toFixed(1) + ',' + (V[1] + u[1] + z[1]).toFixed(1) + ' ' + (V[0] + z[0]).toFixed(1) + ',' + (V[1] + z[1]).toFixed(1) + '"/>';
      }
    });
    ['A', 'B', 'C'].forEach(function (n, i) {
      var vp = out(T[i], 17), ap = inn(T[i], 0.3);
      svg += '<text class="vx" x="' + vp[0].toFixed(1) + '" y="' + vp[1].toFixed(1) + '">' + n + '</text>';
      svg += '<text class="ang" x="' + ap[0].toFixed(1) + '" y="' + ap[1].toFixed(1) + '">' + esc(fmt(an[i], 1)) + '°</text>';
    });
    var sideEnds = [[1, 2], [2, 0], [0, 1]], names = ['a', 'b', 'c'];
    sideEnds.forEach(function (e, i) {
      var mid = [(T[e[0]][0] + T[e[1]][0]) / 2, (T[e[0]][1] + T[e[1]][1]) / 2], lp = out(mid, 15);
      svg += '<text x="' + lp[0].toFixed(1) + '" y="' + lp[1].toFixed(1) + '">' + names[i] + ' = ' + esc(fmt(s[i], 2)) + '</text>';
    });
    return svg + '</svg>';
  }
  function solHTML(sol, idx, total) {
    var s = sol.s, an = sol.a, area = 0.5 * s[1] * s[2] * Math.sin(an[0] * D2R);
    var html = '<div class="card">';
    html += '<span class="sol-title">' + (total > 1 ? 'Triangle ' + (idx + 1) + ' of ' + total + ' · ' : '') + esc(sol.method) + '</span>';
    if (sol.note) html += '<p class="hint"><b>' + esc(sol.note) + '</b></p>';
    html += drawTri(s, an);
    html += '<div class="scroll"><table><tr><th>Side</th><th>Length</th><th>Opposite angle</th><th>Size</th></tr>';
    ['a', 'b', 'c'].forEach(function (n, i) { html += '<tr><td>' + n + '</td><td>' + esc(fmt(s[i], 4)) + '</td><td>' + 'ABC'.charAt(i) + '</td><td>' + esc(fmt(an[i], 3)) + '°</td></tr>'; });
    html += '</table></div>';
    html += '<div class="answer"><span class="label">Perimeter and area</span><span class="big small">P = ' + esc(fmt(s[0] + s[1] + s[2], 4)) + ' &nbsp; Area = ' + esc(fmt(area, 4)) + '</span></div>';
    html += '<ol class="steps">' + sol.steps.map(function (t) { return '<li>' + esc(t) + '</li>'; }).join('') + '</ol></div>';
    return html;
  }
  function showSolutions(sols) { $('#out-tri').innerHTML = sols.map(function (s, i) { return solHTML(s, i, sols.length); }).join(''); }
  function runRight() {
    try {
      showSolutions(solveRight({ a: getNum('r-a', 'Side a'), b: getNum('r-b', 'Side b'), c: getNum('r-c', 'Side c'), A: getNum('r-A', 'Angle A'), B: getNum('r-B', 'Angle B') }));
    } catch (e) { $('#out-tri').innerHTML = errBox(e); }
  }
  function runAny() {
    try {
      showSolutions(solveGeneral([getNum('g-a', 'Side a'), getNum('g-b', 'Side b'), getNum('g-c', 'Side c')], [getNum('g-A', 'Angle A'), getNum('g-B', 'Angle B'), getNum('g-C', 'Angle C')]));
    } catch (e) { $('#out-tri').innerHTML = errBox(e); }
  }
  $('#solve-right').addEventListener('click', runRight);
  $('#solve-any').addEventListener('click', runAny);
  var triKind = 'right';
  $$('[data-tri]').forEach(function (b) {
    b.addEventListener('click', function () {
      triKind = b.getAttribute('data-tri');
      $$('[data-tri]').forEach(function (x) { x.setAttribute('aria-pressed', x === b ? 'true' : 'false'); });
      $('#form-right').hidden = triKind !== 'right'; $('#form-any').hidden = triKind !== 'any';
      if (triKind === 'right') runRight(); else runAny();
    });
  });
  $$('[data-clear]').forEach(function (b) {
    b.addEventListener('click', function () { b.getAttribute('data-clear').split(',').forEach(function (id) { $('#' + id).value = ''; }); });
  });
  $$('[data-fill]').forEach(function (c) {
    c.addEventListener('click', function () {
      var form = c.closest('[data-form]');
      $$('input', form).forEach(function (i) { i.value = ''; });
      c.getAttribute('data-fill').split(',').forEach(function (kv) { var p = kv.split('='); $('#' + p[0]).value = p[1]; });
      if (form.id === 'form-right') runRight(); else runAny();
    });
  });

  function eqValues() {
    return { a: getNum('e-a', 'a'), f: $('#e-f').value, b: getNum('e-b', 'b'), c: getNum('e-c', 'c'), k: getNum('e-k', 'k'), lo: getNum('e-lo', 'Start of range'), hi: getNum('e-hi', 'End of range') };
  }
  function eqPreview() {
    try {
      var v = eqValues();
      $('#e-preview').textContent = fmt(v.a) + ' · ' + v.f + '(' + fmt(v.b) + 'x + ' + fmt(v.c) + '°) = ' + fmt(v.k);
    } catch (e) { $('#e-preview').textContent = ''; }
  }
  function runEq() {
    var out = $('#out-eq');
    try {
      var v = eqValues();
      if (v.a === null || v.b === null || v.c === null || v.k === null || v.lo === null || v.hi === null) throw new Error('Fill in every box.');
      var r = solveEquation(v), html = '<div class="card"><h2>Solution</h2>';
      if (r.none) {
        html += '<div class="answer"><span class="label">Result</span><span class="big small">No solution</span></div>';
      } else {
        html += '<div class="answer"><span class="label">General solution (n is any whole number)</span>' + r.general.map(function (g) { return '<span class="big small">' + esc(g) + '</span>'; }).join('') + '</div>';
        var shown = r.list.slice(0, 60);
        html += '<div class="answer"><span class="label">Solutions from ' + esc(fmt(v.lo)) + '° to ' + esc(fmt(v.hi)) + '°</span><span class="big small">' + (shown.length ? esc(shown.map(function (x) { return fmt(x, 4) + '°'; }).join(',  ') + (r.list.length > 60 ? ', …' : '')) : 'none in this range') + '</span></div>';
      }
      html += '<ol class="steps">' + r.steps.map(function (t) { return '<li>' + esc(t) + '</li>'; }).join('') + '</ol></div>';
      out.innerHTML = html;
    } catch (e) { out.innerHTML = errBox(e); }
  }
  $('#solve-eq').addEventListener('click', runEq);
  ['e-a', 'e-f', 'e-b', 'e-c', 'e-k'].forEach(function (id) { $('#' + id).addEventListener('input', eqPreview); $('#' + id).addEventListener('change', eqPreview); });
  $$('[data-eq]').forEach(function (c) {
    c.addEventListener('click', function () {
      var p = c.getAttribute('data-eq').split(',');
      $('#e-a').value = p[0]; $('#e-f').value = p[1]; $('#e-b').value = p[2]; $('#e-c').value = p[3]; $('#e-k').value = p[4];
      $('#e-lo').value = '0'; $('#e-hi').value = '360';
      eqPreview(); runEq();
    });
  });

  function runIdentity() {
    var out = $('#out-id');
    try {
      var L = parse($('#i-l').value, mode), R = parse($('#i-r').value, mode);
      var pts = mode === 'deg' ? [7, 19, 28, 37, 49, 58, 66, 73, 84, 101, 117, 133, 152, 171, 199, 223, 247, 281, 305, 329, 347]
                               : [0.12, 0.33, 0.51, 0.78, 0.95, 1.2, 1.37, 1.9, 2.3, 2.7, 3.3, 3.9, 4.4, 5.0, 5.6, 6.1];
      var rows = [], bad = 0, valid = 0, i;
      for (i = 0; i < pts.length; i++) {
        var l = L({ x: pts[i] }), r = R({ x: pts[i] });
        if (!isFinite(l) || !isFinite(r)) continue;
        valid++;
        var diff = Math.abs(l - r), tol = 1e-9 * (1 + Math.max(Math.abs(l), Math.abs(r)));
        if (diff > tol) bad++;
        rows.push({ x: pts[i], l: l, r: r, ok: diff <= tol });
      }
      var html = '<div class="card"><h2>Result</h2>';
      if (valid < 5) {
        html += '<div class="err" role="alert"><b>Not enough points:</b> the expressions were undefined at too many test angles to judge.</div></div>';
      } else if (!bad) {
        html += '<div class="answer"><span class="label">Verdict</span><span class="big small ok">Holds at all ' + valid + ' test angles</span></div><p class="hint">This is a numerical check, so it supports the identity without proving it.</p>';
      } else {
        html += '<div class="answer"><span class="label">Verdict</span><span class="big small bad">Not an identity: the sides differ at ' + bad + ' of ' + valid + ' test angles</span></div>';
      }
      if (valid >= 5) {
        var sorted = rows.filter(function (r) { return !r.ok; }).concat(rows.filter(function (r) { return r.ok; })).slice(0, 6);
        html += '<div class="scroll"><table><tr><th>x</th><th>Left</th><th>Right</th><th>Match</th></tr>' + sorted.map(function (r) {
          return '<tr><td>' + esc(fmt(r.x, 3)) + (mode === 'deg' ? '°' : ' rad') + '</td><td>' + esc(fmt(r.l, 6)) + '</td><td>' + esc(fmt(r.r, 6)) + '</td><td class="' + (r.ok ? 'ok' : 'bad') + '">' + (r.ok ? 'yes' : 'no') + '</td></tr>';
        }).join('') + '</table></div></div>';
      }
      out.innerHTML = html;
    } catch (e) { out.innerHTML = errBox(e); }
  }
  $('#solve-id').addEventListener('click', runIdentity);
  $$('[data-id]').forEach(function (c) {
    c.addEventListener('click', function () {
      var p = c.getAttribute('data-id').split('|');
      $('#i-l').value = p[0]; $('#i-r').value = p[1]; runIdentity();
    });
  });

  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Enter' || !e.target || e.target.tagName !== 'INPUT') return;
    var form = e.target.closest('[data-form]');
    var btn = form && $('[data-solve]', form);
    if (btn) { e.preventDefault(); btn.click(); }
  });

  runExpr(); runRight(); eqPreview(); runEq(); runIdentity();
})();
