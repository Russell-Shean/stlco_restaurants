import re, requests, bs4
import pandas as pd


# load data
all_facilities = pd.read_csv("data/all_facilities.csv", sep="\t")



# process data ---------------------------------------------


# pull out facility ID --------------------
fa_ids = []

for link in all_facilities["link"]:
	fa_ids.append(re.compile("(?<=facid\\=).*").findall(link)[0])

all_facilities['facility_id'] = fa_ids








base_url = "https://pressagent.envisionconnect.com/"

first_url = base_url + all_facilities["link"][1]

print(first_url)


res = requests.get(first_url)

# parse html 
soup = bs4.BeautifulSoup(res.text, "html.parser")

# Define html elements for facility name, address, and a link to its inspection history
# This isn't great, we're using the column width to find different fields....

facility_name = soup.find_all("td",attrs={"width": "65%"})[0].getText()

addresses = soup.find_all("td",attrs={"width": "35%"})

address_line1 = addresses[0].getText()
address_line2 = addresses[1].getText()





# Find individual inspections
inspection_dates = soup.find_all("td",attrs={"width": "15%"})

for date in inspection_dates:
	print(f'~~{date.getText()}~~')





print(facility_name)
print(address_line1)
print(address_line2)














###########

# Background knowledge



# The link redirects


# https://pressagent.envisionconnect.com/fac.phtml?agency=stl&violsortfield=TB_CORE_INSPECTION_VIOL.VIOLATION_CODE&facid=FA0004805

#facid=FA0004805


# it starts with a facility ID and ends with a record IT
# https://pressagent.envisionconnect.com/insp.phtml?agency=stl&violsortfield=TB_CORE_INSPECTION_VIOL.VIOLATION_CODE&record_id=PR0000297




