import math

raw_data_nq = """
[info] NQueensBenchmark.capsicumNQueens                 10         N/A  avgt   10     6.138 ±   0.064  ms/op
[info] NQueensBenchmark.capsicumNQueens                 12         N/A  avgt   10   174.813 ±   1.384  ms/op
[info] NQueensBenchmark.capsicumNQueens                 14         N/A  avgt   10  6732.510 ± 209.809  ms/op
[info] NQueensBenchmark.vanillaNQueens                  10         N/A  avgt   10     5.621 ±   0.017  ms/op
[info] NQueensBenchmark.vanillaNQueens                  12         N/A  avgt   10   154.399 ±   2.592  ms/op
[info] NQueensBenchmark.vanillaNQueens                  14         N/A  avgt   10  6231.121 ± 153.467  ms/op
"""

raw_data_pent = """
[info] PentominoBenchmark.capsicumPentomino          5x12  avgt   10  1714.667 ± 31.904  ms/op
[info] PentominoBenchmark.capsicumPentomino          6x10  avgt   10  4533.595 ± 65.693  ms/op
[info] PentominoBenchmark.vanillaPentomino           5x12  avgt   10  1438.564 ±  9.572  ms/op
[info] PentominoBenchmark.vanillaPentomino           6x10  avgt   10  4669.581 ± 26.010  ms/op
"""

def parse_benchmark_to_latex(data):
    capsicum = {}
    vanilla = {}
    
    for line in data.strip().split('\n'):
        if '±' not in line:
            continue
            
        parts = line.split()
            
        bench_name_qualified = parts[1]
        param = parts[2]
        
        try:
            pm_idx = parts.index('±')
            score = float(parts[pm_idx - 1])
            error = float(parts[pm_idx + 1])
        except (ValueError, IndexError):
            continue
        
        if 'capsicum' in bench_name_qualified.lower():
            capsicum[param] = (score, error)
        elif 'vanilla' in bench_name_qualified.lower():
            vanilla[param] = (score, error)

    def sort_key(x):
        try:
            return (0, int(x))
        except ValueError:
            return (1, x)

    n_values = sorted(capsicum.keys(), key=sort_key)
    
    coords = []
    diffs = []
    error_pluses = []
    error_minuses = []
    
    for n in n_values:
        if n not in vanilla:
            continue
        c_score, c_err = capsicum[n]
        v_score, v_err = vanilla[n]
        
        # Core Percentage Difference
        diff_pct = ((c_score - v_score) / v_score) * 100
        diffs.append(diff_pct)
        
        # Max/Min variance ratios (Worst-case bounds approach)
        diff_max = (((c_score + c_err) / (v_score - v_err)) - 1) * 100
        diff_min = (((c_score - c_err) / (v_score + v_err)) - 1) * 100
        
        err_plus = diff_max - diff_pct
        err_minus = diff_pct - diff_min
        
        error_pluses.append(err_plus)
        error_minuses.append(err_minus)
        
        # PGFPlots Asymmetric Error Syntax: (x, y) += (0, y_err_plus) -= (0, y_err_minus)
        coords.append(f"({n}, {diff_pct:.2f}) += (0, {err_plus:.2f}) -= (0, {err_minus:.2f})")

    if not diffs:
        return "% Error: No matching Capsicum/Vanilla pairs parsed successfully."

    # Compute bounds tracking the worst-case error extensions
    max_val_with_error = max(d + ep for d, ep in zip(diffs, error_pluses))
    min_val_with_error = min(d - em for d, em in zip(diffs, error_minuses))
    max_abs = max(abs(max_val_with_error), abs(min_val_with_error))
    
    # Wider Y-axis Range: Multiples of 5 with an explicit +5 unit safety gap
    y_bound = math.ceil((max_abs + 5) / 5.0) * 5
    if y_bound == 0: 
        y_bound = 5
        
    min_n = n_values[0]
    max_n = n_values[-1]

    # Adaptive X-Axis Handling (Numeric vs Symbolic)
    is_numeric_x = all(x.isdigit() for x in n_values)
    if is_numeric_x:
        x_axis_config = f"xtick={{{','.join(n_values)}}}"
        xlabel_text = "$n$ (Board Size)"
    else:
        x_axis_config = f"symbolic x coords={{{','.join(n_values)}}},\n    xtick=data"
        xlabel_text = "Configuration"

    # Generate LaTeX code
    latex_template = f"""\\documentclass[Main.tex]{{subfiles}}
\\usepackage{{pgfplots}}
\\pgfplotsset{{compat=1.18}}

\\begin{{document}}

\\begin{{figure}}[ht]
\\centering
\\begin{{tikzpicture}}
\\begin{{axis}}[
    title={{Performance Overhead Analysis}},
    xlabel={{{xlabel_text}}},
    ylabel={{\\% Difference in Execution Time}},
    {x_axis_config},
    ymin=-{y_bound},
    ymax={y_bound},
    grid=major,
    nodes near coords,
    nodes near coords align={{vertical}},
    nodes near coords style={{/pgf/number format/.cd, fixed, fixed zerofill, precision=2}},
]

% Grey dotted zero-line spanning across the graph space
\\draw[gray, dotted, very thick] (axis cs:{min_n},0) -- (axis cs:{max_n},0);

\\addplot[
    color=blue,
    mark=square*,
    thick,
    error bars/.cd,
    y dir=both,
    y explicit
] coordinates {{
    {' '.join(coords)}
}};

\\end{{axis}}
\\end{{tikzpicture}}
\\caption{{Execution time percentage difference with confidence interval bounds}}
\\end{{figure}}

\\end{{document}}"""
    return latex_template

if __name__ == "__main__":
    latex_output = parse_benchmark_to_latex(raw_data_nq)
    print(latex_output)