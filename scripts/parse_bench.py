import sys
import os
from collections import defaultdict

def parse_benchmark_file(filepath, categories):
    # Data structure: data[category][tool][round_num] = (score, error)
    data = defaultdict(lambda: defaultdict(dict))
    
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            # Skip empty lines or header lines
            if not line.startswith("[info]") or "Mode" in line:
                continue
            
            parts = line.split()
            if len(parts) < 8:
                continue
                
            # parts expected: ['[info]', 'Cdown.CatsCore', 'thrpt', '10', '1129.030', '±', '78.585', 'ops/s']
            name_token = parts[1]
            score = float(parts[4])
            error = float(parts[6])
            
            # Split Category and Tool
            if '.' not in name_token:
                continue
            category, rest = name_token.split('.', 1)

            if categories is not None and category not in categories:
                continue
            
            # Check for rounds (e.g., Fs2__0)
            if '__' in rest:
                tool, round_str = rest.rsplit('__', 1)
                round_num = int(round_str)
            else:
                tool = rest
                round_num = 0
                
            # Escape underscores in the tool name so LaTeX doesn't parse them as math subscripts
            tool_latex = tool.replace('_', '\\_')
            
            data[category][tool_latex][round_num] = (score, error)
            
    return data

def generate_latex(data):
    latex_out = [
        "\\documentclass{article}",
        "\\usepackage{pgfplots}",
        "\\pgfplotsset{compat=1.18}",
        "\\begin{document}\n"
    ]
    
    # Sort categories alphabetically
    for category in sorted(data.keys()):
        tools = data[category]
        # Sort tools alphabetically for the y-axis
        sorted_tools = sorted(tools.keys())
        symbolic_coords = ", ".join(sorted_tools)
        
        # Determine the maximum number of rounds in this specific category
        max_rounds = max([max(rounds.keys()) for rounds in tools.values()]) + 1
        y_spacing = (max_rounds * 0.3) + 0.4
        
        
        latex_out.append("\\begin{figure}[htbp]")
        latex_out.append("\\centering")
        latex_out.append("\\begin{tikzpicture}")
        latex_out.append("\\begin{axis}[")
        latex_out.append("    xbar,")
        latex_out.append("    y dir=reverse, % Forces A to top, Z to bottom")
        latex_out.append(f"    title={{{category}}},")
        latex_out.append("    xlabel={Throughput (ops/s)},")
        latex_out.append(f"    symbolic y coords={{{symbolic_coords}}},")
        latex_out.append("    ytick=data,")
        latex_out.append("    enlarge y limits=0.05,")
        latex_out.append("    width=0.75\\textwidth,")
        latex_out.append("    scaled x ticks=false,")
        latex_out.append("    tick label style={/pgf/number format/fixed},")
        latex_out.append("    bar width=4pt,")
        latex_out.append("    y=0.6cm, % Sets exact distance between each tool")
        latex_out.append(f"    y={y_spacing}cm,")
        latex_out.append("    bar width=0.3cm,")
        latex_out.append("    enlarge y limits={abs=0.6cm},")
        
        if max_rounds > 1:
            latex_out.append("    legend pos=south east,")
            latex_out.append("    area legend,")
            latex_out.append("    reverse legend, % ADD THIS: Fixes the legend order")
            
        latex_out.append("]")
        
        # Generate an \addplot for every round
        for r in reversed(range(max_rounds)):
            plot_lines = [
                "\\addplot+[",
                "    error bars/.cd,",
                "        x dir=both,",
                "        x explicit",
                "] coordinates {"
            ]
            
            has_data = False
            for tool in sorted_tools:
                if r in tools[tool]:
                    score, error = tools[tool][r]
                    plot_lines.append(f"    ({score:.3f},{tool}) +- ({error:.3f},0)")
                    has_data = True
                    
            plot_lines.append("};")
            if max_rounds > 1:
                plot_lines.append(f"\\addlegendentry{{Round {r}}}")
                
            if has_data:
                latex_out.extend(plot_lines)
                
        latex_out.append("\\end{axis}")
        latex_out.append("\\end{tikzpicture}")
        latex_out.append(f"\\caption{{Throughput comparison of the {category} benchmark. Higher is better.}}")
        latex_out.append("\\end{figure}\n")
        
    latex_out.append("\\end{document}")
    
    return "\n".join(latex_out)

def copy_to_clipboard(text):
    import subprocess
    try:
        process = subprocess.Popen(
            ['xclip', '-selection', 'clipboard'], 
            stdin=subprocess.PIPE, 
            text=True
        )
        process.communicate(input=text)
    except FileNotFoundError:
        print("Error: xclip is not installed or not in your PATH.")

if __name__ == "__main__":
    categories_kv = {
        'state': ['Cdown', 'Mulst', 'Sumh'],
        'stream': ['Fmf', 'Crc'],
        'reint': ['Reint'],
    }
    input_filename = sys.argv[1]
    categories = categories_kv[sys.argv[2].lower()] if 2 < len(sys.argv) else None
    
    if os.path.exists(input_filename):
        parsed_data = parse_benchmark_file(input_filename, categories)
        output = generate_latex(parsed_data)
        copy_to_clipboard(output)
        print(output)

    else:
        print(f"Error: Could not find {input_filename} in the current directory.")
