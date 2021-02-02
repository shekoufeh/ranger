/*-------------------------------------------------------------------------------
 This file is part of ranger.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of ranger is distributed under MIT license and the
 R package "ranger" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#include <fstream>
#include <sstream>
#include <stdexcept>
#include <algorithm>
#include <iterator>

#include "Data.h"
#include "utility.h"

namespace ranger {

Data::Data() :
    num_rows(0), num_rows_rounded(0), num_cols(0), snp_data(0), num_cols_no_snp(0), externalData(true), index_data(0), max_num_unique_values(
        0), order_snps(false) {
}

size_t Data::getVariableID(const std::string& variable_name) const {
  auto it = std::find(variable_names.cbegin(), variable_names.cend(), variable_name);
  if (it == variable_names.cend()) {
    throw std::runtime_error("Variable " + variable_name + " not found.");
  }
  return (std::distance(variable_names.cbegin(), it));
}

// #nocov start (cannot be tested anymore because GenABEL not on CRAN)
void Data::addSnpData(unsigned char* snp_data, size_t num_cols_snp) {
  num_cols = num_cols_no_snp + num_cols_snp;
  num_rows_rounded = roundToNextMultiple(num_rows, 4);
  this->snp_data = snp_data;
}
// #nocov end

// #nocov start
bool Data::loadFromFile(std::string filename, std::vector<std::string>& dependent_variable_names) {

  bool result;

  // Open input file
  std::ifstream input_file;
  input_file.open(filename);
  if (!input_file.good()) {
    throw std::runtime_error("Could not open input file.");
  }

  // Count number of rows
  size_t line_count = 0;
  std::string line;
  while (getline(input_file, line)) {
    ++line_count;
  }
  num_rows = line_count - 1;
  input_file.close();
  input_file.open(filename);

  // Check if comma, semicolon or whitespace seperated
  std::string header_line;
  getline(input_file, header_line);

  // Find out if comma, semicolon or whitespace seperated and call appropriate method
  if (header_line.find(",") != std::string::npos) {
    result = loadFromFileOther(input_file, header_line, dependent_variable_names, ',');
  } else if (header_line.find(";") != std::string::npos) {
    result = loadFromFileOther(input_file, header_line, dependent_variable_names, ';');
  } else {
    result = loadFromFileWhitespace(input_file, header_line, dependent_variable_names);
  }

  externalData = false;
  input_file.close();
  return result;
}

bool Data::loadFromFileWhitespace(std::ifstream& input_file, std::string header_line,
    std::vector<std::string>& dependent_variable_names) {

  size_t num_dependent_variables = dependent_variable_names.size();
  std::vector<size_t> dependent_varIDs;
  dependent_varIDs.resize(num_dependent_variables);

  // Read header
  std::string header_token;
  std::stringstream header_line_stream(header_line);
  size_t col = 0;
  while (header_line_stream >> header_token) {
    bool is_dependent_var = false;
    for (size_t i = 0; i < dependent_variable_names.size(); ++i) {
      if (header_token == dependent_variable_names[i]) {
        dependent_varIDs[i] = col;
        is_dependent_var = true;
      }
    }
    if (!is_dependent_var) {
      variable_names.push_back(header_token);
    }
    ++col;
  }

  num_cols = variable_names.size();
  num_cols_no_snp = num_cols;

  // Read body
  reserveMemory(num_dependent_variables);
  bool error = false;
  std::string line;
  size_t row = 0;
  while (getline(input_file, line)) {
    double token;
    std::stringstream line_stream(line);
    size_t column = 0;
    while (readFromStream(line_stream, token)) {
      size_t column_x = column;
      bool is_dependent_var = false;
      for (size_t i = 0; i < dependent_varIDs.size(); ++i) {
        if (column == dependent_varIDs[i]) {
          set_y(i, row, token, error);
          is_dependent_var = true;
          break;
        } else if (column > dependent_varIDs[i]) {
          --column_x;
        }
      }
      if (!is_dependent_var) {
        set_x(column_x, row, token, error);
      }
      ++column;
    }
    if (column > (num_cols + num_dependent_variables)) {
      throw std::runtime_error(
          std::string("Could not open input file. Too many columns in row ") + std::to_string(row) + std::string("."));
    } else if (column < (num_cols + num_dependent_variables)) {
      throw std::runtime_error(
          std::string("Could not open input file. Too few columns in row ") + std::to_string(row)
              + std::string(". Are all values numeric?"));
    }
    ++row;
  }
  num_rows = row;
  return error;
}

bool Data::loadFromFileOther(std::ifstream& input_file, std::string header_line,
    std::vector<std::string>& dependent_variable_names, char seperator) {

  size_t num_dependent_variables = dependent_variable_names.size();
  std::vector<size_t> dependent_varIDs;
  dependent_varIDs.resize(num_dependent_variables);

  // Read header
  std::string header_token;
  std::stringstream header_line_stream(header_line);
  size_t col = 0;
  while (getline(header_line_stream, header_token, seperator)) {
    bool is_dependent_var = false;
    for (size_t i = 0; i < dependent_variable_names.size(); ++i) {
      if (header_token == dependent_variable_names[i]) {
        dependent_varIDs[i] = col;
        is_dependent_var = true;
      }
    }
    if (!is_dependent_var) {
      variable_names.push_back(header_token);
    }
    ++col;
  }

  num_cols = variable_names.size();
  num_cols_no_snp = num_cols;

  // Read body
  reserveMemory(num_dependent_variables);
  bool error = false;
  std::string line;
  size_t row = 0;
  while (getline(input_file, line)) {
    std::string token_string;
    double token;
    std::stringstream line_stream(line);
    size_t column = 0;
    while (getline(line_stream, token_string, seperator)) {
      std::stringstream token_stream(token_string);
      readFromStream(token_stream, token);

      size_t column_x = column;
      bool is_dependent_var = false;
      for (size_t i = 0; i < dependent_varIDs.size(); ++i) {
        if (column == dependent_varIDs[i]) {
          set_y(i, row, token, error);
          is_dependent_var = true;
          break;
        } else if (column > dependent_varIDs[i]) {
          --column_x;
        }
      }
      if (!is_dependent_var) {
        set_x(column_x, row, token, error);
      }
      ++column;
    }
    ++row;
  }
  num_rows = row;
  return error;
}
// #nocov end

double Data::compute_median(std::vector<double> &values) const
{
  size_t size = values.size();
  
  if (size == 0)
  {
    return 0;  // Undefined.
  }
  else
  {
    std::sort(values.begin(), values.end());
    if (size % 2 == 0)
    {
      return (values[size / 2 - 1] + values[size / 2]) / 2;
    }
    else 
    {
      return values[size / 2];
    }
  }
}


std::vector<double> Data::get_median_imputed_unique_vector(double &out_median, std::vector<size_t> &sampleIDs,
                                       size_t split_varID, size_t start_pos, size_t end_pos) const
{
  size_t pos = start_pos;
  bool missVal = false;
  double sampleVal;
  std::vector<double> nonMissingVals;
  while (pos < end_pos) {
    size_t sampleID = sampleIDs[pos];
    sampleVal = get_x(sampleID, split_varID);
    missVal = std::isnan(sampleVal);
    if(!missVal){
      nonMissingVals.push_back(sampleVal);
    }
    ++pos;
  }
  // for median imputation
  double median = compute_median(nonMissingVals);
  out_median = median;
  
  // create sorted unique_vector, first add the imputed data
  nonMissingVals.push_back(median);
  //std::vector<double> unique_values(nonMissingVals)
  auto cmp=[](const double &d1, const double &d2){
    if (std::isnan(d1)) return false;
    if (std::isnan(d2)) return true;
    return d1<d2;
  };  
  std::sort(nonMissingVals.begin(), nonMissingVals.end(),cmp);
  nonMissingVals.erase(unique(nonMissingVals.begin(), nonMissingVals.end()), nonMissingVals.end());
  
  return nonMissingVals;
}

double Data::get_median_of_non_missing(std::vector<size_t> &sampleIDs,
                                         size_t split_varID, size_t start_pos, size_t end_pos) const
{
  size_t pos = start_pos;
  bool missVal = false;
  double sampleVal;
  std::vector<double> nonMissingVals;
  while (pos < end_pos) {
    size_t sampleID = sampleIDs[pos];
    sampleVal = get_x(sampleID, split_varID);
    missVal = std::isnan(sampleVal);
    if(!missVal){
      nonMissingVals.push_back(sampleVal);
    }
    ++pos;
  }
  // for median imputation
  double median = compute_median(nonMissingVals);
  
  return median;
}

void Data::getAllValues(std::vector<double>& all_values, std::vector<size_t>& sampleIDs, size_t varID, size_t start,
    size_t end) const {

  // All values for varID (no duplicates) for given sampleIDs
  if (getUnpermutedVarID(varID) < num_cols_no_snp) {
    
    all_values.reserve(end - start);
    for (size_t pos = start; pos < end; ++pos) {
      all_values.push_back(get_x(sampleIDs[pos], varID));
    }
	auto cmp=[](const double &d1, const double &d2){
      if (std::isnan(d1)) return false;
      if (std::isnan(d2)) return true;
      return d1<d2;
    }; 												
    std::sort(all_values.begin(), all_values.end(),cmp);
    all_values.erase(std::unique(all_values.begin(), all_values.end()), all_values.end());
  } else {
    // If GWA data just use 0, 1, 2
    all_values = std::vector<double>( { 0, 1, 2 });
  }
}

void Data::getMinMaxValues(double& min, double&max, std::vector<size_t>& sampleIDs, size_t varID, size_t start,
    size_t end) const {
  if (sampleIDs.size() > 0) {
    min = get_x(sampleIDs[start], varID);
    max = min;
  }
  for (size_t pos = start; pos < end; ++pos) {
    double value = get_x(sampleIDs[pos], varID);
    if (value < min) {
      min = value;
    }
    if (value > max) {
      max = value;
    }
  }
}

void Data::sort() {
  
  // Reserve memory
  index_data.resize(num_cols_no_snp * num_rows);
  
  // For all columns, get unique values and save index for each observation
  for (size_t col = 0; col < num_cols_no_snp; ++col) {
    
    // Get all unique values
    std::vector<double> unique_values(num_rows);
    for (size_t row = 0; row < num_rows; ++row) {
      unique_values[row] = get_x(row, col);
    }
    auto cmp=[](const double &d1, const double &d2){
      if (std::isnan(d1)) return false;
      if (std::isnan(d2)) return true;
      return d1<d2;
    };  
    std::sort(unique_values.begin(), unique_values.end(),cmp);
    unique_values.erase(unique(unique_values.begin(), unique_values.end()), unique_values.end());
    
    // Get index of unique value
    for (size_t row = 0; row < num_rows; ++row) {
      size_t idx = std::lower_bound(unique_values.begin(), unique_values.end(), get_x(row, col))
      - unique_values.begin();
      index_data[col * num_rows + row] = idx;
    }
    
    // Save unique values
    unique_data_values.push_back(unique_values);
    if (unique_values.size() > max_num_unique_values) {
      max_num_unique_values = unique_values.size();
    }
  }
}

void Data::imputeAndSort(uint imputation_method){
  
  // Reserve memory
  index_data.resize(num_cols_no_snp * num_rows);
  bool missingValExists;
  bool nonMissingValExists;
  double value;
  std::vector<double> nonMissingValues;
  std::vector<size_t> missingLocations;
  // For all columns, get unique values and save index for each observation
  for (size_t col = 0; col < num_cols_no_snp; ++col) {
    missingValExists = false;
    nonMissingValExists = false;
    nonMissingValues.clear();
    missingLocations.clear();
    // Get all unique values
    std::vector<double> unique_values(num_rows);
    for (size_t row = 0; row < num_rows; ++row) {
      value = get_x(row, col);
      if(std::isnan(value)){
        missingValExists = true;
        missingLocations.push_back(row);
      }else{
        nonMissingValExists = true;
        nonMissingValues.push_back(value);
      }
      unique_values[row] = value;
    }
    std::vector<double> unique_imputed_values(unique_values);
    
    // replace missing with 
    
    if(imputation_method == 1){
      double median = compute_median(nonMissingValues);
      
      // copy the median in missing locations
      size_t endIt = missingLocations.size();
      for (size_t row = 0; row < endIt; ++row) {
        unique_imputed_values[missingLocations[row]] = median; 
      }
      all_imputed_data.push_back(unique_imputed_values);
     }
    
    /////////////////////////////////////////////
    // Perform sorting based on the original data
    /////////////////////////////////////////////
    auto cmp=[](const double &d1, const double &d2){
      if (std::isnan(d1)) return false;
      if (std::isnan(d2)) return true;
      return d1<d2;
    };  
    std::sort(unique_values.begin(), unique_values.end(),cmp);
    unique_values.erase(unique(unique_values.begin(), unique_values.end()), unique_values.end());
    
    // Get index of unique value
    for (size_t row = 0; row < num_rows; ++row) {
      size_t idx = std::lower_bound(unique_values.begin(), unique_values.end(), get_x(row, col))
      - unique_values.begin();
      index_data[col * num_rows + row] = idx;
    }
    // Save unique values
    unique_data_values.push_back(unique_values);
    if (unique_values.size() > max_num_unique_values) {
      max_num_unique_values = unique_values.size();
    }
    
    ////////////////////////////////////////
    // Perform sorting based on imputed data
    ////////////////////////////////////////
    
      
    index_imputed_data.resize(num_cols_no_snp * num_rows);
    //std::vector<double> cpy_unique_imputed_values(unique_imputed_values);
    std::sort(unique_imputed_values.begin(), unique_imputed_values.end(),cmp);
    unique_imputed_values.erase(unique(unique_imputed_values.begin(), unique_imputed_values.end()), unique_imputed_values.end());
    
    // Get index of imputed unique value
    for (size_t row = 0; row < num_rows; ++row) {
      value = get_x(row, col);
      if(std::isnan(value)){
        value = all_imputed_data[col][row];
      }
      size_t idx = std::lower_bound(unique_imputed_values.begin(), unique_imputed_values.end(), value)
      - unique_imputed_values.begin();
      index_imputed_data[col * num_rows + row] = idx;
    }

    // Save unique values
    unique_imputed_data_values.push_back(unique_imputed_values);
    if (unique_imputed_values.size() > max_num_unique_values) {
      max_num_unique_imputed_values = unique_imputed_values.size();
    }
    
  }
  
  
}



// TODO: Implement ordering for multiclass and survival
// #nocov start (cannot be tested anymore because GenABEL not on CRAN)
void Data::orderSnpLevels(bool corrected_importance) {
  // Stop if now SNP data
  if (snp_data == 0) {
    return;
  }

  size_t num_snps;
  if (corrected_importance) {
    num_snps = 2 * (num_cols - num_cols_no_snp);
  } else {
    num_snps = num_cols - num_cols_no_snp;
  }

  // Reserve space
  snp_order.resize(num_snps, std::vector<size_t>(3));

  // For each SNP
  for (size_t i = 0; i < num_snps; ++i) {
    size_t col = i;
    if (i >= (num_cols - num_cols_no_snp)) {
      // Get unpermuted SNP ID
      col = i - num_cols + num_cols_no_snp;
    }

    // Order by mean response
    std::vector<double> means(3, 0);
    std::vector<double> counts(3, 0);
    for (size_t row = 0; row < num_rows; ++row) {
      size_t row_permuted = row;
      if (i >= (num_cols - num_cols_no_snp)) {
        row_permuted = getPermutedSampleID(row);
      }
      size_t idx = col * num_rows_rounded + row_permuted;
      size_t value = (((snp_data[idx / 4] & mask[idx % 4]) >> offset[idx % 4]) - 1);

      // TODO: Better way to treat missing values?
      if (value > 2) {
        value = 0;
      }

      means[value] += get_y(row, 0);
      ++counts[value];
    }

    for (size_t value = 0; value < 3; ++value) {
      means[value] /= counts[value];
    }

    // Save order
    snp_order[i] = order(means, false);
  }

  order_snps = true;
}
// #nocov end

} // namespace ranger

