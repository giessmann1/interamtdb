import pandas as pd
import os
import webbrowser


def frequency_by_columns(df, column1, column2):
    result = df.groupby([column1, column2]).size().reset_index(name='Frequency')
    result = result.sort_values(by='Frequency', ascending=False)
    return result


def frequency_by_one_column(dataframe, column_name):
    counts = dataframe[column_name].value_counts()
    return pd.DataFrame(
        {column_name: counts.index, 'Frequency': counts.values}
    )


def view_df_as_html(df):
    html = df.to_html()
    path = os.path.abspath('temp.html')
    url = 'file://' + path
    with open(path, 'w') as f:
        f.write(html)
    webbrowser.open(url)


class bcolors:
    # https://stackoverflow.com/questions/287871/how-do-i-print-colored-text-to-the-terminal
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'