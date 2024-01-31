from PyPDF2 import PdfReader
import pandas as pd

program_files = [["Wahlprogramme_LTW/Wahlprogramm_Thüringen_2019_Endfassung.pdf", "TH", "2019"],
                ["Wahlprogramme_LTW/20210611_AfD_SH_Programm_2022_Aktuell.pdf", "SH", "2022"],
                ["Wahlprogramme_LTW/Landtagswahl23_Programm_Broschüre_klein-AfD-Bayern-komprimiert.pdf", "BY", "2023"],
                ["Wahlprogramme_LTW/landtagswahlprogramm_afd_2021_a4_print.pdf", "BW", "2021"],
                ["Wahlprogramme_LTW/AfD-RLP_Wahlprogramm_2021_2.1-ansicht.pdf", "RLP", "2021"],
                ["Wahlprogramme_LTW/AfD-NRW-Wahlprogramm-Landtagswahl-NRW-2022.pdf", "NRW", "2022"],
                ["Wahlprogramme_LTW/2023-05-11-Wahlprogramm-LTW23-gemaess-LPT-Melsungen.pdf", "HE", "2023"],
                ["Wahlprogramme_LTW/afd_wahlprogramm_sachsen2019.pdf", "SN", "2019"],
                ["Wahlprogramme_LTW/afdniedersachsenltw22wahlprogramm.pdf", "NI", "2022"],
                ["Wahlprogramme_LTW/Landtagswahlprogram21AfDLSA.pdf", "ST", "2021"],
                ["Wahlprogramme_LTW/Landtagswahlprogramm_AfD_Saarland.pdf", "SL", "2022"],
                ["Wahlprogramme_LTW/Wahlprogramm_Brandenburg_2019_acc2144-01-06-19-final.pdf", "BB", "2019"],
                ["Wahlprogramme_LTW/AfD_Wahlprogramm_LTWMV2021.pdf", "MV", "2021"],
                ["Wahlprogramme_LTW/Bürgerschaftswahlprogramm-2020-der-AfD-Hamburg.pdf", "HH", "2020"],
                ["Wahlprogramme_LTW/20230313_LTW23_Bremen_Programm_Broschuere_DINA5.pdf", "HB", "2023"]]

df = pd.DataFrame(columns=["id", "state", "year", "text"])

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
    df = df.append({'id': prog, 'state': state, 'year': year, 'text': whole_text}, ignore_index=True)

df.to_csv('afd_manifestos.tsv', sep='\t', index=False)