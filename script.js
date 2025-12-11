  function toFraction(value, maxDenominator = 1000) {
    if (!isFinite(value) || Number.isNaN(value)) return "0";
    if (Math.abs(Math.round(value) - value) < 1e-9) return String(Math.round(value));
    let sign = value < 0 ? -1 : 1;
    value = Math.abs(value);
    let best = [0, 1];
    let diff = value;
    for (let d = 1; d <= maxDenominator; d++) {
      let n = Math.round(value * d);
      let newDiff = Math.abs(value - n / d);
      if (newDiff < diff) {
        diff = newDiff;
        best = [n, d];
        if (diff < 1e-12) break;
      }
    }
    let n = best[0] * sign, d = best[1];
    if (d === 1) return String(n);
    return `${n}/${d}`;
  }

  const unicodeFractionMap = {
    "1/2": "½",
    "1/3": "⅓", "2/3": "⅔",
    "1/4": "¼", "3/4": "¾",
    "1/5": "⅕", "2/5": "⅖", "3/5": "⅗", "4/5": "⅘",
    "1/6": "⅙", "5/6": "⅚",
    "1/7": " ⅐ ",
    "1/8": "⅛", "3/8": "⅜", "5/8": "⅝", "7/8": "⅞",
    "1/9": " ⅑ ",
    "1/10": " ⅒ "
  };

  function toUnicodeFractionString(value) {
    if (!isFinite(value) || Number.isNaN(value)) return "0";
    if (Math.abs(Math.round(value) - value) < 1e-9) return String(Math.round(value));
    for (let d = 1; d <= 20; d++) {
      let n = Math.round(value * d);
      if (Math.abs(value - n / d) < 1e-9) {
        const unsignedKey = `${Math.abs(n)}/${d}`;
        if (unicodeFractionMap[unsignedKey]) {
          return (n < 0 ? "-" : "") + unicodeFractionMap[unsignedKey];
        }
        return `${n}/${d}`;
      }
    }
    return Number(value).toFixed(6).replace(/\.?0+$/, "");
  }

  function fmt(n) {
    const num = Number(n);
    return toUnicodeFractionString(num);
  }
  const qs = (s, c = document) => c.querySelector(s);
const qsa = (s, c = document) => Array.from(c.querySelectorAll(s));
const constraintsEl = qs("#constraints");
const maxConstraints = 8;

function coeffsToExpr(coeffs) {
  const parts = [];
  if (coeffs[0] !== 0) {
    const a = coeffs[0];
    parts.push((a === 1 ? "" : a === -1 ? "-" : a) + "x");
  }
  if (coeffs[1] !== 0) {
    const b = coeffs[1];
    const sign = b > 0 && parts.length ? " + " : "";
    parts.push(sign + (Math.abs(b) === 1 ? (b === -1 ? "-" : "") : b) + "y");
  }
  if (parts.length === 0) return "0";
  return parts.join("");
}

function createConstraint(defaultCoeffs = [], defaultRel, defaultRhs) {
  const div = document.createElement("div");
  div.className = "constraint";
  const coeffInput = document.createElement("input");
  coeffInput.type = "text";
  coeffInput.placeholder = "e.g. 2x + 2y";
  if (defaultCoeffs && defaultCoeffs.length) {
    coeffInput.value = coeffsToExpr(defaultCoeffs);
  } else {
    coeffInput.value = "";
  }
  const rel = document.createElement("select");
  rel.innerHTML = `<option value="<=">≤</option><option value=">=">≥</option>`;
  rel.value = defaultRel || "<=";
  const rhs = document.createElement("input");
  rhs.type = "number";
  rhs.placeholder = "RHS";
  rhs.value = defaultRhs !== undefined ? defaultRhs : "";
  const del = document.createElement("button");
  del.innerHTML = " ✕ ";
  del.className = "del";
  del.type = "button";
  del.onclick = () => {
    if (constraintsEl.contains(div)) constraintsEl.removeChild(div);
  };
  div.appendChild(coeffInput);
  div.appendChild(rel);
  div.appendChild(rhs);
  div.appendChild(del);
  return div;
}

function addConstraintUI(coeffs, rel, rhs) {
  if (constraintsEl.children.length >= maxConstraints) return;
  constraintsEl.appendChild(createConstraint(coeffs, rel, rhs));
}

function parseExpression(expr) {
  if (!expr || typeof expr !== "string") return [0, 0];
  expr = expr.replace(/\s+/g, "").toLowerCase();
  expr = expr.replace(/^\+/, "");
  expr = expr.replace(/([^\^\-+])-/g, (m, p1) => p1 + "+-");
  let ax = 0, ay = 0;
  const xMatch = expr.match(/([+-]?\d*\.?\d*)x/);
  const yMatch = expr.match(/([+-]?\d*\.?\d*)y/);
  if (xMatch) {
    const s = xMatch[1];
    if (s === "" || s === "+") ax = 1;
    else if (s === "-") ax = -1;
    else ax = parseFloat(s);
  }
  if (yMatch) {
    const s = yMatch[1];
    if (s === "" || s === "+") ay = 1;
    else if (s === "-") ay = -1;
    else ay = parseFloat(s);
  }
  if (!isFinite(ax)) ax = 0;
  if (!isFinite(ay)) ay = 0;
  return [ax, ay];
}

function resetForm() {
  qs("#obj-x-count").value = 2;
  qs("#obj-coeffs").value = "120x + 100y";
  constraintsEl.innerHTML = "";
  addConstraintUI([2, 2], "<=", 8);
  addConstraintUI([5, 3], "<=", 15);
  qs("#output").innerHTML = "";
}

qs("#add-constraint").onclick = () => addConstraintUI();
qs("#reset").onclick = resetForm;
resetForm();
function buildProblemFromUI() {
  const objText = qs("#obj-coeffs").value.trim();
  if (!objText) throw "Enter objective expression (e.g. 120x + 100y).";
  const objCoeffs = parseExpression(objText);
  const nvars = 2;
  const cons = [];
  qsa(".constraint").forEach((div) => {
    const exprText = div.querySelector("input[type=text]").value.trim();
    const coeffs = parseExpression(exprText);
    const rel = div.querySelector("select").value;
    const rhsVal = div.querySelector("input[type=number]").value;
    const rhs = rhsVal === "" ? NaN : parseFloat(rhsVal);
    if (Number.isNaN(rhs)) throw "Every constraint needs a RHS number.";
    cons.push({ coeffs, rel, rhs });
  });
  if (cons.length === 0) throw "Add at least one constraint.";
  return { nvars, objCoeffs, cons };
}

function canonicalize(problem) {
  const { nvars, objCoeffs, cons } = problem;
  const constraints = cons.map((c) => {
    if (c.rel == ">=") return { coeffs: c.coeffs.map((v) => -v), rhs: -c.rhs, origRel: ">=" };
    return { coeffs: c.coeffs.slice(), rhs: c.rhs, origRel: "<=" };
  });
  return { nvars, objCoeffs: objCoeffs.slice(), constraints };
}

function buildTableau(canon) {
  const m = canon.constraints.length;
  const n = canon.nvars;
  const A = canon.constraints.map((r) => r.coeffs.slice());
  const b = canon.constraints.map((r) => r.rhs);
  const numCols = n + m;
  const T = [];
  for (let i = 0; i < m; i++) {
    const row = new Array(numCols + 1).fill(0);
    for (let j = 0; j < n; j++) row[j] = A[i][j];
    row[n + i] = 1;
    row[numCols] = b[i];
    T.push(row);
  }
  const z = new Array(numCols + 1).fill(0);
  for (let j = 0; j < n; j++) z[j] = -canon.objCoeffs[j];
  T.push(z);
  const basic = [];
  for (let i = 0; i < m; i++) basic.push("s" + (i + 1));
  const varNames = [];
  for (let j = 0; j < n; j++) {
    varNames.push(j === 0 ? "x" : j === 1 ? "y" : "x" + (j + 1));
  }
  for (let j = 0; j < m; j++) varNames.push("s" + (j + 1));
  return { T, basic, varNames, canon };
}

function cloneTable(T) {
  return T.map((r) => r.slice());
}
function runSimplexDetailed(tableauObj) {
  const { T: T0, basic: basic0, varNames, canon } = tableauObj;
  const T = cloneTable(T0);
  const basic = basic0.slice();
  const m = T.length - 1;
  const ncols = T[0].length;
  const eps = 1e-9;
  const log = [];

  log.push({
    type: "initial",
    snapshot: { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice() }
  });
  let iter = 0;
  while (true) {
    iter++;
    const zrow = T[T.length - 1];

    let pivotCol = -1, minVal = -eps;
    for (let j = 0; j < ncols - 1; j++) {
      if (zrow[j] < minVal) {
        minVal = zrow[j];
        pivotCol = j;
      }
    }
    if (pivotCol === -1) {
      log.push({ type: "done", snapshot: { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice() } });
      return { log, status: "optimal", final: { T: cloneTable(T), basic: basic.slice(), varNames } };
    }

    let pivotRow = -1, bestRatio = Infinity;
    const ratios = new Array(m).fill(null);
    const beforeSnapshot = { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice() };
    for (let i = 0; i < m; i++) {
      const a = beforeSnapshot.T[i][pivotCol];
      const rhs = beforeSnapshot.T[i][ncols - 1];
      if (a > eps) {
        const ratio = rhs / a;
        ratios[i] = ratio;
        if (ratio < bestRatio - eps) {
          bestRatio = ratio;
          pivotRow = i;
        }
      } else {
        ratios[i] = null;
      }
    }

    log.push({
      type: "pivot-select",
      snapshot: { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice(), ratios: ratios.map(r => r === null ? "—" : fmt(r)), pivot: { row: pivotRow, col: pivotCol } },
      extra: { pivotCol, pivotRow }
    });
    if (pivotRow === -1) {
      log.push({ type: "unbounded", snapshot: { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice() } });
      return { log, status: "unbounded" };
    }

    const beforeNormalize = cloneTable(beforeSnapshot.T);
    const pivotVal = beforeNormalize[pivotRow][pivotCol];

    const normalizedPivot = beforeNormalize[pivotRow].map((v) => v / pivotVal);

    for (let j = 0; j < ncols; j++) T[pivotRow][j] /= pivotVal;

    const headers = [];
    headers.push(beforeSnapshot.basic[pivotRow]);
    for (let i = 0; i < m; i++) {
      if (i === pivotRow) continue;
      headers.push(beforeSnapshot.basic[i]);
    }
    headers.push("Z");

    log.push({
      type: "elimination-table",
      snapshot: {
        before: beforeNormalize,
        after: cloneTable(T),
        headers,
        pivot: { row: pivotRow, col: pivotCol, pivotVal },
        normalizedPivot,
        varNames: varNames.slice()
      }
    });

    for (let i = 0; i < T.length; i++) {
      if (i === pivotRow) continue;
      const factor = T[i][pivotCol];
      for (let j = 0; j < ncols; j++) {
        T[i][j] -= factor * T[pivotRow][j];
      }
    }

    basic[pivotRow] = varNames[pivotCol];

    log.push({
      type: "post-pivot",
      snapshot: { T: cloneTable(T), basic: basic.slice(), varNames: varNames.slice(), pivot: { row: pivotRow, col: pivotCol } }
    });
    if (iter > 200) {
      log.push({ type: "abort", snapshot: { T: cloneTable(T), basic: basic.slice() } });
      return { log, status: "aborted" };
    }
  }
}
function renderTableauCard(snapshot, title) {
  const { T, basic, varNames, pivot, ratios } = snapshot;
  const m = T.length - 1;
  const ncols = T[0].length;
  const eps = 1e-9;
  let computedRatios = ratios;
  if (pivot && !ratios) {

    computedRatios = new Array(m).fill(null);
    for (let i = 0; i < m; i++) {
      const a = T[i][pivot.col];
      const rhs = T[i][ncols - 1];
      if (a > eps) {
        computedRatios[i] = rhs / a;
      }
    }
    computedRatios = computedRatios.map(r => r === null ? "—" : fmt(r));
  }
  const wrapper = document.createElement("div");
  wrapper.className = "tableau-card";
  wrapper.innerHTML = `<strong>${title}</strong>`;
  const table = document.createElement("table");
  const thead = document.createElement("thead");
  const trh = document.createElement("tr");
  trh.innerHTML = `<th>B.V</th>`;

  for (let j = 0; j < ncols - 1; j++) {
    const th = document.createElement("th");
    th.textContent = varNames[j] || `c${j+1}`;
    trh.appendChild(th);
  }
  trh.innerHTML += `<th>RHS</th><th>Ratio</th>`;
  thead.appendChild(trh);
  table.appendChild(thead);
  const tbody = document.createElement("tbody");
  for (let i = 0; i < m; i++) {
    const tr = document.createElement("tr");
    tr.innerHTML = `<td>${basic ? basic[i] : ""}</td>`;
    for (let j = 0; j < ncols; j++) {
      const td = document.createElement("td");
      td.innerHTML = fmt(T[i][j]);

      if (pivot) {
        if (i === pivot.row && j === pivot.col) td.classList.add("pivot");
        else if (i === pivot.row) td.classList.add("pivot-row");
        else if (j === pivot.col) td.classList.add("pivot-col");
      }
      tr.appendChild(td);
    }
    const ratioTd = document.createElement("td");
    if (computedRatios && computedRatios[i] !== undefined) {
      ratioTd.innerHTML = computedRatios[i];
    } else {
      ratioTd.innerHTML = "";
    }
    tr.appendChild(ratioTd);
    tbody.appendChild(tr);
  }
  const tfoot = document.createElement("tfoot");
  const trz = document.createElement("tr");
  trz.innerHTML = `<td>Z</td>`;
  for (let j = 0; j < ncols; j++) {
    const td = document.createElement("td");
    td.innerHTML = fmt(T[m][j]);
    trz.appendChild(td);
  }
  trz.innerHTML += `<td></td>`;
  tfoot.appendChild(trz);
  table.appendChild(tbody);
  table.appendChild(tfoot);
  wrapper.appendChild(table);
  return wrapper;
}

function renderPivotInfo(pivotCol, pivotRow, pivotVal, varNames, basic, zEntry) {
  const div = document.createElement("div");
  div.className = "verify-card";
  let html = `<strong>Pivot Information</strong><br>`;
  html += `<strong>Pivot column:</strong> ${varNames[pivotCol]} (most negative entry in Z row: ${fmt(zEntry)})<br>`;
  html += `<strong>Pivot row:</strong> ${basic[pivotRow]} (smallest positive ratio)<br>`;
  html += `<strong>Pivot element:</strong> ${fmt(pivotVal)}<br>`;
  html += `<strong>Pivotal elimination:</strong> Multiply 1/k (where k is pivot) to the entries in pivot row`;
  div.innerHTML = html;
  return div;
}

function renderStandardForm(tableauObj) {
  const { canon, varNames } = tableauObj;
  const n = canon.nvars;
  const container = document.createElement("div");
  container.className = "verify-card";
  let html = `<strong>STANDARD FORM</strong><div style="margin-top:10px; font-family:monospace; white-space:pre-line;">`;

  const objParts = canon.objCoeffs.map((c, i) => `${fmt(c)}${varNames[i]}`).join(" + ");
  const zExpr = canon.objCoeffs.map((c, i) => `${fmt(c)}${varNames[i]}`).join(" - ").replace(/- /g, "- ");
  html += `${objParts}  -->  z - ${canon.objCoeffs.map((c, i) => `${fmt(c)}${varNames[i]}`).join(" - ")} = 0\n`;
  for (let i = 0; i < canon.constraints.length; i++) {
    const coeffs = canon.constraints[i].coeffs;
    const left = coeffs.map((coef, j) => `${fmt(coef)}${varNames[j]}`).join(" + ");
    const slackName = varNames[n + i];
    const rhs = canon.constraints[i].rhs;
    html += `${left}  -->  ${left} + ${slackName} = ${fmt(rhs)}\n`;
  }
  const baseVars = varNames.slice(0, n).join(", ");
  const slacks = varNames.slice(n).join(", ");
  html += `${baseVars} >= 0  -->  ${baseVars}, ${slacks} >= 0`;
  html += `</div>`;
  container.innerHTML = html;
  return container;
}

function renderEliminationTable(entry) {
  const { before, after, headers, pivot, normalizedPivot, varNames } = entry.snapshot;
  const m = before.length - 1;
  const ncols = before[0].length;
  const wrapper = document.createElement("div");
  wrapper.className = "tableau-card";
  wrapper.innerHTML = `<strong>Pivotal Elimination Steps</strong>`;
  const table = document.createElement("table");
  table.style.width = "100%";
  table.style.textAlign = "left";

  const thead = document.createElement("thead");
  const thr = document.createElement("tr");
  thr.innerHTML = `<th> </th>`;
  headers.forEach((h) => {
    const th = document.createElement("th");
    th.textContent = h;
    thr.appendChild(th);
  });
  thead.appendChild(thr);
  table.appendChild(thead);
  const tbody = document.createElement("tbody");

  const colLabels = [];
  for (let j = 0; j < ncols - 1; j++) colLabels.push(varNames[j] || `c${j+1}`);
  colLabels.push("RHS");

  for (let j = 0; j < ncols; j++) {
    const tr = document.createElement("tr");

    const leftTd = document.createElement("td");
    leftTd.innerHTML = colLabels[j];
    tr.appendChild(leftTd);

    const pivotCell = document.createElement("td");
    pivotCell.innerHTML = `${fmt(before[pivot.row][j])}(1/${fmt(pivot.pivotVal)}) = ${fmt(normalizedPivot[j])}`;
    tr.appendChild(pivotCell);

    for (let i = 0; i < m; i++) {
      if (i === pivot.row) continue;
      const before_i_j = before[i][j];
      const factor_i = before[i][pivot.col];
      const newVal_i_j = after[i][j];
      const td = document.createElement("td");
      td.innerHTML = `${fmt(before_i_j)} - ${fmt(factor_i)}(${fmt(normalizedPivot[j])}) = ${fmt(newVal_i_j)}`;
      tr.appendChild(td);
    }

    const beforeZ_j = before[m][j];
    const factorZ = before[m][pivot.col];
    const newZ_j = after[m][j];
    const zTd = document.createElement("td");
    zTd.innerHTML = `${fmt(beforeZ_j)} - ${fmt(factorZ)}(${fmt(normalizedPivot[j])}) = ${fmt(newZ_j)}`;
    tr.appendChild(zTd);
    tbody.appendChild(tr);
  }
  table.appendChild(tbody);
  wrapper.appendChild(table);
  return wrapper;
}

function renderDetailedLog(log) {
  const container = document.createElement("div");
  container.style.marginTop = "10px";
  let tableauCount = 0;
  for (let i = 0; i < log.length; i++) {
    const entry = log[i];

    if (entry.type === "initial" || entry.type === "post-pivot") {
      tableauCount++;
      
      const nextEntry = log[i + 1];

      if (nextEntry && nextEntry.type === "pivot-select") {
        continue;
      }
 
      container.appendChild(renderTableauCard(entry.snapshot, `Tableau ${tableauCount}`));
      continue;
    }

    if (entry.type === "pivot-select") {
 
      container.appendChild(renderTableauCard(entry.snapshot, `Tableau ${tableauCount}`));

      const pivotCol = entry.extra.pivotCol;
      const pivotRow = entry.extra.pivotRow;
      const varNames = entry.snapshot.varNames || log[0].snapshot.varNames;
      const basic = entry.snapshot.basic || log[0].snapshot.basic;
      const zEntry = entry.snapshot.T ? entry.snapshot.T[entry.snapshot.T.length - 1][pivotCol] : 0;
      const pivotVal = entry.snapshot.T && pivotRow >= 0 ? entry.snapshot.T[pivotRow][pivotCol] : 0;
      container.appendChild(renderPivotInfo(pivotCol, pivotRow, pivotVal, varNames, basic, zEntry));
      continue;
    }

    if (entry.type === "elimination-table") {
      container.appendChild(renderEliminationTable(entry));
      continue;
    }
  }
  return container;
}

function renderSolutionFromFinal(finalObj, problem) {
  const final = finalObj;
  const sol = {};
  final.varNames.forEach((v) => (sol[v] = 0));
  const T = final.T;
  const ncols = T[0].length;
  const m = T.length - 1;
  final.basic.forEach((v, i) => {
    if (v in sol) sol[v] = T[i][ncols - 1];
  });
  const container = document.createElement("div");
  container.className = "solution";
  const varsToShow = [];
  for (let i = 0; i < problem.nvars; i++) {
    const name = i === 0 ? "x" : i === 1 ? "y" : "x" + (i + 1);
    varsToShow.push(name);
  }
  const lines = varsToShow.map((v) => v + "=" + fmt(sol[v] || 0)).join("    ");
  container.innerHTML = `<strong>Optimal Solution Found</strong><br>${lines}<br><strong>Z=${fmt(T[m][ncols - 1])}</strong>`;
  return container;
}

function renderSolutionVerificationFromFinal(finalObj, problem) {
  const final = finalObj;
  const T = final.T;
  const ncols = T[0].length;
  const m = T.length - 1;
  const sol = {};
  final.varNames.forEach((v) => (sol[v] = 0));
  final.basic.forEach((v, i) => {
    if (v in sol) sol[v] = T[i][ncols - 1];
  });
  const varNames = [];
  for (let i = 0; i < problem.nvars; i++) varNames.push(i === 0 ? "x" : i === 1 ? "y" : "x" + (i + 1));
  const wrapper = document.createElement("div");
  wrapper.className = "verify-card";
  let html = `<h3 style="color:#ff512f; margin:0 0 8px 0;">Solution Verification</h3>`;
  html += `<strong>Optimal Solution:</strong> ${varNames.map((v) => `${v}=${fmt(sol[v] || 0)}`).join(", ")}<br>`;
  html += `<strong>Z = ${fmt(T[m][ncols - 1])}</strong><br><br>`;
  html += `<div style="margin-bottom:10px;">`;
  problem.cons.forEach((c, idx) => {
    const aCoeffs = c.coeffs;
    let lhs = 0;
    for (let k = 0; k < problem.nvars; k++) lhs += (aCoeffs[k] || 0) * (sol[varNames[k]] || 0);
    const relation = c.rel;
    const RHS = c.rhs;
    const holds = relation === "<=" ? lhs <= RHS + 1e-9 : lhs >= RHS - 1e-9;
    const eqLeft = aCoeffs.slice(0, problem.nvars).map((coef, i) => `${coef}${varNames[i]}`).join(" + ") || "0";
    html += `<div style="margin-bottom:12px;">`;
    html += `<strong>${eqLeft} ${relation} ${RHS}</strong><br>`;
    html += `${aCoeffs.slice(0, problem.nvars).map((coef, i) => `${coef}(${fmt(sol[varNames[i]] || 0)})`).join(" + ")}<br>`;
    html += `${aCoeffs.slice(0, problem.nvars).map((coef, i) => fmt(coef * (sol[varNames[i]] || 0))).join(" + ")}<br>`;
    html += `<strong>${fmt(lhs)} ${relation} ${fmt(RHS)}</strong> &nbsp; <strong style="color:${holds ? "#2f71ffe6" : "red"}">${holds ? "True" : "False"}</strong>`;
    html += `</div>`;
  });
  html += `</div>`;
  html += `<div><strong>Objective:</strong> `;
  const objParts = problem.objCoeffs.map((coef, i) => `${coef}${varNames[i] || "x" + (i + 1)}`).join(" + ");
  html += `${objParts}<br>`;
  const objValue = problem.objCoeffs.reduce((s, coef, i) => s + coef * (sol[varNames[i]] || 0), 0);
  html += `${problem.objCoeffs.map((coef, i) => `${coef}(${fmt(sol[varNames[i]] || 0)})`).join(" + ")}<br>`;
  html += `<strong>${fmt(objValue)} = ${fmt(T[m][ncols - 1])}</strong> &nbsp; <strong style="color: #2f71ffe6">True</strong>`;
  html += `</div>`;
  wrapper.innerHTML = html;
  return wrapper;
}
qs("#solve").onclick = () => {
  try {
    qs("#output").innerHTML = "";
    const problem = buildProblemFromUI();
    const canon = canonicalize(problem);
    const tableauObj = buildTableau(canon);

    qs("#output").appendChild(renderStandardForm(tableauObj));

    const result = runSimplexDetailed(tableauObj);

    qs("#output").appendChild(renderDetailedLog(result.log));
    if (result.status === "unbounded") {
      const unb = document.createElement("div");
      unb.className = "verify-card";
      unb.innerHTML = `<strong style="color: red;">Unbounded</strong><br>The objective can increase without bound in the chosen direction.`;
      qs("#output").appendChild(unb);
      return;
    }
    if (result.status === "optimal") {
      const finalSnapshot = result.final;

      qs("#output").appendChild(renderTableauCard(finalSnapshot, "Final Tableau (Optimal)"));
      qs("#output").appendChild(renderSolutionFromFinal(finalSnapshot, problem));

      const verify = renderSolutionVerificationFromFinal(finalSnapshot, problem);
      if (verify) qs("#output").appendChild(verify);
    } else {
      const note = document.createElement("div");
      note.className = "verify-card";
      note.innerHTML = `<strong>Note:</strong> Solver ended with status '${result.status}'.`;
      qs("#output").appendChild(note);
    }
  } catch (err) {
    qs("#output").innerHTML = `<div class="verify-card"><strong style="color:red">Error:</strong> ${String(err)}</div>`;
  }
};