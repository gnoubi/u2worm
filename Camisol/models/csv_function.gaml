/**
* Name: csvfunction
*  
* Author: Lucas GROSJEAN
* 
* Library function for csv file used in camisole model
*/


model csvfunction

global
{
	
	csv_file apport_organique;
	csv_file exportation_NPK;
	matrix data_apport_csv;
	matrix data_NPK_csv;
	
	init
	{
		do setup;
	}
	
	action setup
	{
		apport_organique <- csv_file("../includes/qualite_amendements_organiques_csv.csv");
		exportation_NPK <- csv_file("../includes/exportation_NPK_csv.csv");
		
		do matrix_conversion;
	}
	
	action matrix_conversion
	{
		data_apport_csv <- matrix(apport_organique);
		data_NPK_csv <- matrix(exportation_NPK);
	}
	
	// OM -> Organic Matter
	/**
	 * search_OM : return index of the line of a given MO or -1 if it don't exist in file
	 * 
	 * 
	 * @param : name_OM : name of organic matter to search for
	 * 
	 * @return : index or -1;
	 */
	int search_OM(string name_OM)
	{
		string lower_case_OM <- lower_case(name_OM);
		loop i from: 0 to: data_apport_csv.rows - 1
		{
			string lower_case_data <- lower_case(data_apport_csv[0,i] as string);
			if(lower_case_data = lower_case_OM){
				//write("found "+name_OM+ " at line " + i);
				return i;
			}
		}
		
		write(""+name_OM+" not found in file");
		return -1;
	}
	
	/* for all function get_YYYYYYY
	 * 
	 * return value of YYYYYY or nil if name_OM dont exist
	 * 
	 * @param name_OM : name of organic matter to get value of YYYYYYY
	 * 
	 * @return value of YYYYY or nil if name_OM dont exist
	*/
	
	
	/* for all function get_YYYYYYY_with_index
	 * 
	 * return value of YYYYYY or nil if index is wrong
	 * 
	 * @param name_OM : name of organic matter to get value of YYYYYYY
	 * 
	 * @return value of YYYYY or nil if index is wrong
	*/

	
	string get_name_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[0, index_line] as string;
		}
	}
	
	float get_soluble(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[1, index_line] as float;
		}
	}
	
	float get_soluble_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[1, index_line] as float;
		}
	}
	
	float get_hemicellulose(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[2, index_line] as float;
		}
	}
	
	float get_hemicellulose_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[2, index_line] as float;
		}
	}
	
	float get_celluose(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[3, index_line] as float;
		}
	}
	
	float get_celluose_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[3, index_line] as float;
		}
	}
	
	float get_lignine(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[4, index_line] as float;
		}
	}
	
	float get_lignine_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[4, index_line] as float;
		}
	}
	
	
	float get_CN(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[5, index_line] as float;
		}
	}
	
	float get_CN_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[5, index_line] as float;
		}
	}
	
	
	float get_CP(string name_OM)
	{
		int index_line <- search_OM(name_OM);
		if(index_line != -1){
			return data_apport_csv[6, index_line] as float;
		}
	}
	
	float get_CP_with_index(int index_line)
	{
		if(index_line >= 0 and index_line < data_apport_csv.rows)
		{
			return data_apport_csv[6, index_line] as float;
		}
	}	
	
	
	
	// NPK + harvest index
	
	int search_index_species(string name_species)
	{
		string lower_case_OM <- lower_case(name_species);
		loop i from: 0 to: data_NPK_csv.rows - 1
		{
			string lower_case_data <- lower_case(data_NPK_csv[0,i] as string);
			if(lower_case_data = lower_case_OM){
				write("found "+name_species+ " at line " + i);
				return i;
			}
		}
		
		write(""+name_species+" not found in file");
		return -1;
	}
	
	float get_N(int index)
	{
		if(index >= 0 and index < data_NPK_csv.rows)
		{
			return data_NPK_csv[1, index] as float;
		}
	}
	float get_P(int index)
	{
		if(index >= 0 and index < data_NPK_csv.rows)
		{
			return data_NPK_csv[2, index] as float;
		}
	}
	float get_HI(int index)
	{
		if(index >= 0 and index < data_NPK_csv.rows)
		{
			return data_NPK_csv[3, index] as float;
		}
	}


}