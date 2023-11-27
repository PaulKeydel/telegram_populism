import asyncio
from datetime import datetime, timedelta
from telethon import TelegramClient, events
import pandas as pd
import telegram_account as acc

#groups and links
group_links = [["https://t.me/afdbrennpunkt", "Bund"],
               ["https://t.me/afd_thl", "TH"],
               ["https://t.me/afdfraktionnrw", "NRW"]]


async def get_group_messages(group_names: list, days_back: int, hours_delay = 0) -> list:
    #create a Telegram client with the specified API ID, API hash and phone number
    client = TelegramClient('session_name', acc.api_id, acc.api_hash)
    await client.connect()

    #check if the user is already authorized, otherwise prompt the user to authorize the client
    if not await client.is_user_authorized():
        await client.send_code_request(acc.phone_number)
        await client.sign_in(acc.phone_number, input('Enter the code: '))
    
    #specify time range
    start_time = datetime.utcnow().replace(minute=0, second=0, microsecond=0)
    end_time = start_time - timedelta(days=days_back)
    if (hours_delay > 0):
        start_time = start_time - timedelta(hours=hours_delay)

    #collect messages in all groups
    msglist = list()
    for gridx in range(len(group_names)):
        #get the ID of the specified group
        group = await client.get_entity(group_names[gridx][0])
        messages = []
        async for message in client.iter_messages(group):
            #compare the dates after removing time zone
            if (message.date.replace(tzinfo=None) < end_time):
                break
            if (message.date.replace(tzinfo=None) > start_time):
                continue
            messages.append(message)
        msglist.append(messages)
    return msglist

def count_all_reactions(reactions):
    if (reactions == None):
        return pd.NA
    num = 0
    for i in range(len(reactions.results)):
        num += reactions.results[i].count
    return num

def get_sender_of_fwd_msg(forward) -> str:
    if (forward != None):
        if (forward.chat.username != None):
            return forward.chat.username
    return ""


grmessages = asyncio.run(get_group_messages(group_links, days_back=20, hours_delay=24))

#append new messages to CSV file
df = pd.read_csv('telegram_data.tsv', header=0, sep='\t')
for gridx in range(len(group_links)):
    grmsg = grmessages[gridx]
    avail_time_stamps = (df[df['state'] == group_links[gridx][1]])['creat_time']
    for i in range(len(grmsg)):
        #exclude messages that contain only media
        if (str(grmsg[i].text) == ""):
            continue
        creation_date = grmsg[i].date.replace(tzinfo=None)
        if not(avail_time_stamps.map(str).eq(str(creation_date)).any()):
            df = df.append({'creat_time': creation_date,
                            'state': group_links[gridx][1],
                            'fw': get_sender_of_fwd_msg(grmsg[i].forward),
                            'text': str(grmsg[i].text).replace('\t', ''),
                            'likes': count_all_reactions(grmsg[i].reactions),
                            'views': grmsg[i].views,
                            'age': (datetime.utcnow() - creation_date).total_seconds() / 3600.0}, ignore_index=True)
            print("msg (" + str(creation_date) + ") added")

df.to_csv('telegram_data.tsv', sep='\t', index=False)
print(df)