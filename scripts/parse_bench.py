import sys
import os
from collections import defaultdict

def parse_benchmark_file(filepath, categories):
    # Data structure: data[category][tool][round_num] = (score, error)
    data = defaultdict(lambda: defaultdict(dict))
    
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line.startswith("[info]") or "Mode" in line:
                continue
            
            parts = line.split()
            if len(parts) < 8:
                continue
                
            name_token = parts[1]
            score = float(parts[4])
            error = float(parts[6])
            
            if '.' not in name_token:
                continue
            category, rest = name_token.split('.', 1)

            if categories is not None and category not in categories:
                continue
            
            if '__' in rest:
                tool, round_str = rest.rsplit('__', 1)
                round_num = int(round_str)
            else:
                tool = rest
                round_num = 0
                
            tool_latex = tool.replace('_', '\\_')
            data[category][tool_latex][round_num] = (score, error)
            
    return data

def generate_latex(data):
    latex_out = [
        "\\documentclass{article}",
        "\\usepackage{pgfplots}",
        "\\usepackage{graphicx}",
        "\\usepackage{xcolor}",
        "\\usepackage[margin=1in]{geometry}",
        "\\pgfplotsset{compat=1.18}",
        "",
        "% Custom palette extracted from user image",
        "\\definecolor{imgblue}{HTML}{4F81BD}",
        "\\definecolor{imgteal}{HTML}{65B3B3}",
        "\\definecolor{imggreen}{HTML}{9EBC59}",
        "\\definecolor{imgorange}{HTML}{E89D4F}",
        "\\definecolor{imgslate}{HTML}{45455E}",
        "\\definecolor{imgred}{HTML}{B95858}",
        "\\begin{document}\n"
    ]
    
    latex_out.append("\\begin{figure}[!htbp]")
    latex_out.append("\\centering")
    
    categories = sorted(data.keys())
    num_categories = len(categories)
    
    for i, category in enumerate(categories):
        tools = data[category]
        sorted_tools = sorted(tools.keys())
        symbolic_coords = ", ".join(sorted_tools)
        
        max_rounds = max([max(rounds.keys()) for rounds in tools.values()]) + 1
        
        # CHANGED: Caps the maximum width so single graphs don't become massive
        width_fraction = min(0.95 / num_categories, 0.45)
        
        latex_out.append(f"\\begin{{minipage}}[b]{{{width_fraction:.2f}\\textwidth}}")
        latex_out.append("  \\centering")
        latex_out.append("  \\resizebox{\\textwidth}{!}{%")
        latex_out.append("  \\begin{tikzpicture}")
        latex_out.append("  \\begin{axis}[")
        latex_out.append("      ybar,")
        latex_out.append(f"      title={{{category}}},")
        latex_out.append("      ylabel={Throughput (ops/s)},")
        latex_out.append(f"      symbolic x coords={{{symbolic_coords}}},")
        latex_out.append("      xtick=data,")
        latex_out.append("      x tick label style={rotate=90, anchor=east, font=\\Large, text width=4.5cm, align=right},") 
        latex_out.append("      enlarge x limits={abs=0.8cm},")
        latex_out.append("      ymin=0,")
        latex_out.append("      width=12cm,")
        latex_out.append("      height=16cm,") 
        latex_out.append("      scaled y ticks=false,")
        latex_out.append("      y tick label style={/pgf/number format/fixed, font=\\large, text width=2cm, align=right},")
        latex_out.append("      bar width=8pt,") 
        
        if max_rounds > 1:
            latex_out.append("      legend pos=north east,")
            latex_out.append("      legend style={font=\\large},")
            latex_out.append("      area legend,")
            
        latex_out.append("  ]")
        
        for r in range(max_rounds):
            if max_rounds == 1:
                color_options = "fill=imgblue, draw=imgblue!70!black"
            else:
                palette = [
                    "fill=imgteal, draw=imgteal!70!black",
                    "fill=imggreen, draw=imggreen!70!black",
                    "fill=imgorange, draw=imgorange!70!black",
                    "fill=imgslate, draw=imgslate!70!black",
                    "fill=imgred, draw=imgred!70!black"
                ]
                color_options = palette[r % len(palette)]

            plot_lines = [
                "  \\addplot+[",
                f"      {color_options},", 
                "      error bars/.cd,",
                "          y dir=both,",
                "          y explicit",
                "  ] coordinates {"
            ]
            
            has_data = False
            for tool in sorted_tools:
                if r in tools[tool]:
                    score, error = tools[tool][r]
                    plot_lines.append(f"      ({tool},{score:.3f}) +- (0,{error:.3f})")
                    has_data = True
                    
            plot_lines.append("  };")
            if max_rounds > 1:
                plot_lines.append(f"  \\addlegendentry{{Round {r}}}")
                
            if has_data:
                latex_out.extend(plot_lines)
                
        latex_out.append("  \\end{axis}")
        latex_out.append("  \\end{tikzpicture}%")
        latex_out.append("  }") 
        latex_out.append("\\end{minipage}")
        
        if i < num_categories - 1:
            latex_out.append("\\hfill")
            
    latex_out.append("\\vspace{0.5cm}")
    latex_out.append("\\caption{Throughput comparison of benchmarks. Higher is better.}")
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
