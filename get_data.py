import requests, bs4, re, json, math
import pandas as pd




def get_facilities(page_url):

	'''
    This function returns all the facilities found on a single page



	'''

    # download html for report page
	res = requests.get(page_url)

	# parse html 
	soup = bs4.BeautifulSoup(res.text, "html.parser")

    # Define html elements for facility name, address, and a link to its inspection history
	address_elements = soup.select(".bodytext > td:nth-child(2)")
	link_elements = soup.select(".bodytext > td:nth-child(1) > a")
	name_elements = soup.select(".bodytext > td:nth-child(1)")


    # pull all the names, addresses and links as separate lists
	addresses = []

	for address in address_elements:
		addresses.append(address.getText())


	links = []	

	for link in link_elements:
		links.append(link["href"])

	names = []

	for name in name_elements:
		names.append(name.getText())


	#print(f'number of facilities:{len(names)}')

    
    # combine the lists into a dictionary and then make it a pandas dataframe 
	facilities = {"name":names,
	              "address":addresses,
	              "link":links
	              }



	facilities_df =  pd.DataFrame(facilities)


	return(facilities_df)



def count_facilities(zip_code):


	'''
	This function counts all the results and generates links for each individual page of results. 
	It then calls the function above to build a massive dataframe containing all facilities in Saint Louis County

	'''




	# define url to first page

	base_url = 'https://pressagent.envisionconnect.com/results.phtml?agency=stl&violsortfield=TB_CORE_INSPECTION_VIOL.VIOLATION_CODE'
	search_params = f'&offset=0&businessname=&businessstreet=&city=&zip={zip_code}&facilityid=&FTS=&soundslike=&sort=FACILITY_NAME'

	first_url = base_url + search_params

    # pull html from first page
	res = requests.get(first_url)

	# parse html 
	soup = bs4.BeautifulSoup(res.text, "html.parser")

    # find the two top rows
	rows = soup.find_all("td",attrs={"valign": "top"})

	
    # find number of results for the zip code
	results_count = rows[1].getText()


    # pull the number out of the free text
	n_results = int(re.compile(".*(?=matches)").findall(results_count)[0].strip())

    
	print(f'{n_results} were found in zip code {zip_code}')

	# There are 50 pages of results per page
	# the five key on my keyboard doesn't work and I was to lazy to run the shell script 
	# to remap it to the print key
	# which is why it's 60 -10 here lol

	n_pages = math.ceil(n_results / (60-10))

    # define an empty list for all the individual page urls
	page_urls = []

	for page in range(n_pages):
		offset = 50 * page

		
		search_params = f'&offset={offset}&businessname=&businessstreet=&city=&zip={zip_code}&facilityid=&FTS=&soundslike=&sort=FACILITY_NAME'

		page_urls.append(base_url + search_params)

    

    # loop through all the pages of results and pull all the facility info


    #create a new empty dataframe
	facilities_list = []

	for url in page_urls:

		facilities_list.append( get_facilities(url) )


	return(facilities_list)







# Shape file of stl co zip codes: https://data.stlouisco.com/datasets/zipcodes-stlouiscounty/explore

# list of zip codes in Saint Louis County
#zip_codes = [63145, 63049, 63074, 63114, 63043, 63146, 63131, 63011, 63026, 
#63088, 63033, 63135, 63034, 63031, 63042, 63134, 63140, 63040, 
#63021, 63044, 63045, 63038, 63025, 63069, 63141, 63124, 63132, 
#63129, 63128, 63127, 63144, 63123, 63125, 63111, 63147, 63121, 
#63120, 63138, 63137, 63136, 63105, 63117, 63119, 63143, 63139, 
#63130, 63133, 63126, 63122, 63005, 63017]



# looks like a list of zip codes is unnecesary, all facilities are returned with any zipcode value

zip_code = 63

# concatenate the list of facilities into a single dataframe
all_facilities_df = pd.concat(count_facilities(zip_code))

# save to csv
all_facilities_df.to_csv("./data/all_facilities.csv", sep='\t', encoding='utf-8', index=False, header=True)



