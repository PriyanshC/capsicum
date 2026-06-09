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
        "\\documentclass[Main.tex]{subfiles}",
        "\\begin{document}\n"
    ]
    
    latex_out.append("\\begin{figure}[!htbp]")
    latex_out.append("\\makebox[\\textwidth][c]{")
    
    categories = sorted(data.keys())
    tikz_plots = []

    legend_kv = {
        'mulst': ['1 state', '2 states', '3 states', '4 states', '5 states'],
        'reint': ['100 queries/batch', '1,000 queries/batch', '10,000 queries/batch'],
        'crc': ['Chunk size = 2032', 'Chunk size = 496', 'Chunk size = 112', 'Chunk size = 48', 'Unchunked'],
        'nqueensbenchmark': ['N = 8', 'N = 10', 'N = 12'],
    }
    
    num_plots = len(categories)
    if num_plots == 1:
        plot_scale = 0.9   
    elif num_plots == 2:
        plot_scale = 0.75   
    else:
        plot_scale = 0.5    
    
    for category in categories:
        tools = data[category]
        sorted_tools = sorted(tools.keys())
        symbolic_coords = ", ".join(sorted_tools)
        
        max_rounds = max([max(rounds.keys()) for rounds in tools.values()]) + 1
        
        bar_width = 4
        bar_gap = 1
        tool_gap = 12 
        
        group_width = max_rounds * bar_width + max(0, max_rounds - 1) * bar_gap
        x_step = group_width + tool_gap
        enlarge_x = (group_width / 2) + 10
        
        plot_lines = []
        plot_lines.append(f"  \\begin{{tikzpicture}}[scale={plot_scale}, transform shape, baseline=(current axis.south)]")
        plot_lines.append("  \\begin{axis}[")
        plot_lines.append(f"      ybar={bar_gap}pt,")
        plot_lines.append(f"      title={{{category}}},")
        plot_lines.append("      ylabel={Throughput (ops/s)},")
        plot_lines.append(f"      symbolic x coords={{{symbolic_coords}}},")
        plot_lines.append("      xtick=data,")
        plot_lines.append("      x tick label style={rotate=45, anchor=north east, font=\\footnotesize},") 
        plot_lines.append(f"      enlarge x limits={{abs={enlarge_x}pt}},")
        plot_lines.append("      enlarge y limits={upper, value=0.5},") 
        plot_lines.append("      ymin=0,")
        plot_lines.append(f"      x={x_step}pt,") 
        plot_lines.append("      height=6cm,") 
        plot_lines.append("      scaled y ticks=false,")
        plot_lines.append("      y tick label style={/pgf/number format/fixed, font=\\large, text width=2cm, align=right},")
        plot_lines.append(f"      bar width={bar_width}pt,") 
        
        if max_rounds > 1:
            plot_lines.append("      legend pos=north east,")
            plot_lines.append("      legend cell align=left,") 
            
            # --- FIX: Force smaller font and tighten vertical spacing ---
            plot_lines.append("      legend style={font=\\scriptsize, row sep=-2pt},")
            
            plot_lines.append("      area legend,")
            
        plot_lines.append("  ]")
        
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

            addplot = [
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
                    addplot.append(f"      ({tool},{score:.3f}) +- (0,{error:.3f})")
                    has_data = True
                    
            addplot.append("  };")
            if max_rounds > 1:
                addplot.append(f"  \\addlegendentry{{{legend_kv[category.lower()][r]}}}")
                
            if has_data:
                plot_lines.extend(addplot)
                
        plot_lines.append("  \\end{axis}")
        plot_lines.append("  \\end{tikzpicture}%")
        
        tikz_plots.append("\n".join(plot_lines))

    for i, plot in enumerate(tikz_plots):
        latex_out.append(plot)
        if i < len(tikz_plots) - 1:
            latex_out.append("\\hspace{0.5cm}%")   
            
    latex_out.append("}")
    
    latex_out.append("\\vspace{0.5cm}") 
    latex_out.append(f"\\caption{{Throughput comparison of benchmarks {', '.join(categories)}}}")
    latex_out.append(f"\\label{{eval:{'_'.join(categories).lower()}}}")
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
    
    if len(sys.argv) < 2:
        print("Usage: python script.py <input_filename> [category_group]")
        sys.exit(1)
        
    input_filename = sys.argv[1]
    categories = categories_kv[sys.argv[2].lower()] if 2 < len(sys.argv) else None
    
    if os.path.exists(input_filename):
        parsed_data = parse_benchmark_file(input_filename, categories)
        output = generate_latex(parsed_data)
        copy_to_clipboard(output)
        print("LaTeX copied to clipboard successfully.")

    else:
        print(f"Error: Could not find {input_filename} in the current directory.")