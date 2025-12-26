#!/usr/bin/env python3
import pandas as pd

def check_study1():
    df = pd.read_csv('20250820_Analysis & Results/20250820_combined_dataset.csv')
    study1 = df[df['Study_ID'] == 1].iloc[0]
    print('=== STUDY 1 QUOTE SAMPLES ===')
    
    for col in df.columns:
        if 'Supporting_quotes_for' in col and pd.notna(study1[col]):
            print(f'\n{col}:')
            raw_value = str(study1[col])
            print(f'  Raw: {repr(raw_value[:200])}...')
            
            # Parse like our highlighter does
            quotes = []
            for line in raw_value.split('\n'):
                line = line.strip()
                # Check different bullet patterns
                prefixes = ['- "', '• "', '* "']
                for prefix in prefixes:
                    if line.startswith(prefix) and line.endswith('"'):
                        quote = line[len(prefix):-1]  # Remove prefix and trailing quote
                        if quote:
                            quotes.append(quote)
                        break
            
            print(f'  Parsed quotes ({len(quotes)}):')
            for i, q in enumerate(quotes[:3], 1):
                print(f'    {i}. {repr(q[:100])}...' if len(q) > 100 else f'    {i}. {repr(q)}')
            if len(quotes) > 3:
                print(f'    ... and {len(quotes)-3} more')

if __name__ == '__main__':
    check_study1()
