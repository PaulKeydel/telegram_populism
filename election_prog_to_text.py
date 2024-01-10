from PyPDF2 import PdfReader
import pandas as pd

program_files = [["Wahlprogramme_LTW/Wahlprogramm_Thüringen_2019_Endfassung.pdf", "TH", "2019"],
                ["Wahlprogramme_LTW/20210611_AfD_SH_Programm_2022_Aktuell.pdf", "SH", "2021"],
                ["Wahlprogramme_LTW/Landtagswahl23_Programm_Broschüre_klein-AfD-Bayern-komprimiert.pdf", "BY", "2023"]]

df = pd.DataFrame(columns=["state", "year", "text"])

for prog in range(len(program_files)):
    #creating a pdf reader object
    reader = PdfReader(program_files[prog][0])

    whole_text = ""
    for page in range(len(reader.pages)):
        text = reader.pages[page].extract_text()
        if text != "":
            whole_text = whole_text + "\n" + text

    whole_text = whole_text.replace('\t', ' ')
    state = program_files[prog][1]
    year = program_files[prog][2]
    df = df.append({'state': state, 'year': year, 'text': whole_text}, ignore_index=True)

df.to_csv('afd_wahlprogramm.tsv', sep='\t', index=False)