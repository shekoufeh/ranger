/*-------------------------------------------------------------------------------
 This file is part of ranger.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of ranger is distributed under MIT license and the
 R package "ranger" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#include <iterator>

#include "Tree.h"
#include "utility.h"

namespace ranger {

Tree::Tree() :
    mtry(0), num_samples(0), num_samples_oob(0), min_node_size(0), deterministic_varIDs(0), split_select_weights(0), case_weights(
        0), manual_inbag(0), oob_sampleIDs(0), holdout(false), keep_inbag(false), data(0), regularization_factor(0), regularization_usedepth(
        false), split_varIDs_used(0), variable_importance(0), importance_mode(DEFAULT_IMPORTANCE_MODE), sample_with_replacement(
        true), sample_fraction(0), memory_saving_splitting(false), splitrule(DEFAULT_SPLITRULE), alpha(DEFAULT_ALPHA), minprop(
        DEFAULT_MINPROP), num_random_splits(DEFAULT_NUM_RANDOM_SPLITS), max_depth(DEFAULT_MAXDEPTH), depth(0), last_left_nodeID(
        0) {
}

Tree::Tree(std::vector<std::vector<size_t>>& child_nodeIDs, std::vector<size_t>& split_varIDs,
    std::vector<double>& split_values, std::vector<double>& imputed_values) :
    mtry(0), num_samples(0), num_samples_oob(0), min_node_size(0), deterministic_varIDs(0), split_select_weights(0), case_weights(
        0), manual_inbag(0), split_varIDs(split_varIDs), split_values(split_values), imputed_values(imputed_values),child_nodeIDs(child_nodeIDs), oob_sampleIDs(
        0), holdout(false), keep_inbag(false), data(0), regularization_factor(0), regularization_usedepth(false), split_varIDs_used(
        0), variable_importance(0), importance_mode(DEFAULT_IMPORTANCE_MODE), sample_with_replacement(true), sample_fraction(
        0), memory_saving_splitting(false), splitrule(DEFAULT_SPLITRULE), alpha(DEFAULT_ALPHA), minprop(
        DEFAULT_MINPROP), num_random_splits(DEFAULT_NUM_RANDOM_SPLITS), max_depth(DEFAULT_MAXDEPTH), depth(0), last_left_nodeID(
        0) {
}

void Tree::init(const Data* data, uint mtry, size_t num_samples, uint seed, std::vector<size_t>* deterministic_varIDs,
    std::vector<double>* split_select_weights, ImportanceMode importance_mode, uint min_node_size,
    bool sample_with_replacement, bool memory_saving_splitting, SplitRule splitrule, std::vector<double>* case_weights,
    std::vector<size_t>* manual_inbag, bool keep_inbag, std::vector<double>* sample_fraction, double alpha,
    double minprop, bool holdout, uint num_random_splits, uint max_depth, std::vector<double>* regularization_factor,
    bool regularization_usedepth, std::vector<bool>* split_varIDs_used,
	double missing_tree_weight, uint imputation_method) {	 

  this->data = data;
  this->mtry = mtry;
  this->num_samples = num_samples;
  this->memory_saving_splitting = memory_saving_splitting;

  // Create root node, assign bootstrap sample and oob samples
  child_nodeIDs.push_back(std::vector<size_t>());
  child_nodeIDs.push_back(std::vector<size_t>());
  createEmptyNode();

  // Initialize random number generator and set seed
  random_number_generator.seed(seed);

  this->deterministic_varIDs = deterministic_varIDs;
  this->split_select_weights = split_select_weights;
  this->importance_mode = importance_mode;
  this->min_node_size = min_node_size;
  this->sample_with_replacement = sample_with_replacement;
  this->splitrule = splitrule;
  this->case_weights = case_weights;
  this->manual_inbag = manual_inbag;
  this->keep_inbag = keep_inbag;
  this->sample_fraction = sample_fraction;
  this->holdout = holdout;
  this->alpha = alpha;
  this->minprop = minprop;
  this->num_random_splits = num_random_splits;
  this->max_depth = max_depth;
  this->regularization_factor = regularization_factor;
  this->regularization_usedepth = regularization_usedepth;
  this->split_varIDs_used = split_varIDs_used;

  this->missing_count.resize(num_samples, 0);
  
  this->missing_tree_weight = missing_tree_weight;			
  this->imputation_method = imputation_method;
  
  // Regularization
  if (regularization_factor->size() > 0) {
    regularization = true;
  } else {
    regularization = false;
  }
}

void Tree::grow(std::vector<double>* variable_importance) {
  // Allocate memory for tree growing
  allocateMemory();

  this->variable_importance = variable_importance;

  // Bootstrap, dependent if weighted or not and with or without replacement
  if (!case_weights->empty()) {
    if (sample_with_replacement) {
      bootstrapWeighted();
    } else {
      bootstrapWithoutReplacementWeighted();
    }
  } else if (sample_fraction->size() > 1) {
    if (sample_with_replacement) {
      bootstrapClassWise();
    } else {
      bootstrapWithoutReplacementClassWise();
    }
  } else if (!manual_inbag->empty()) {
    setManualInbag();
  } else {
    if (sample_with_replacement) {
      bootstrap();
    } else {
      bootstrapWithoutReplacement();
    }
  }

  // Init start and end positions
  start_pos[0] = 0;
  end_pos[0] = sampleIDs.size();

  // While not all nodes terminal, split next node
  size_t num_open_nodes = 1;
  size_t i = 0;
  depth = 0;
  while (num_open_nodes > 0) {
    // Split node
    bool is_terminal_node = splitNode(i);
    if (is_terminal_node) {
      --num_open_nodes;
    } else {
      ++num_open_nodes;
      if (i >= last_left_nodeID) {
        // If new level, increase depth
        // (left_node saves left-most node in current level, new level reached if that node is splitted)
        last_left_nodeID = split_varIDs.size() - 2;
        ++depth;
      }
    }
    ++i;
  }

  // Delete sampleID vector to save memory
  sampleIDs.clear();
  sampleIDs.shrink_to_fit();
  cleanUpInternal();
}

void Tree::predict(const Data* prediction_data, bool oob_prediction) {
  std::uniform_real_distribution<double> distribution(0.0,1.0);
  size_t num_samples_predict;
  if (oob_prediction) {
    num_samples_predict = num_samples_oob;
  } else {
    num_samples_predict = prediction_data->getNumRows();
  }

  prediction_terminal_nodeIDs.resize(num_samples_predict, 0);
  prediction_terminal_node_missing_counts.resize(num_samples_predict,0);
  
  // randomly pick a number btw 0, 1
  bool missGoLeft = false;
  bool missVal = false;
  
  // For each sample start in root, drop down the tree and return final value																	   
  for (size_t i = 0; i < num_samples_predict; ++i) {
    size_t sample_idx;
    if (oob_prediction) {
      sample_idx = oob_sampleIDs[i];
    } else {
      sample_idx = i;
    }
    size_t nodeID = 0;
	  size_t missing_count = 0;						 
    while (1) {

      // Break if terminal node
      if (child_nodeIDs[0][nodeID] == 0 && child_nodeIDs[1][nodeID] == 0) {
        break;
      }
 
      
      // Move to child
      size_t split_varID = split_varIDs[nodeID];

      double value = prediction_data->get_x(sample_idx, split_varID);
      
      
	    // randomly pick a number btw 0, 1
      missGoLeft = false;
      missVal = false;
      
      missVal = std::isnan(value);
      
      double newValueForMissing;
      if(imputation_method == 1 && missVal){
        // compute the median of non-missing values
        //newValueForMissing = 1.0;
        
        newValueForMissing = imputed_values[nodeID];
        //newValueForMissing = data->get_median_of_non_missing(sampleIDs, split_varID, start_pos[nodeID], end_pos[nodeID]);
      }
      if(missVal){
        if(imputation_method==0){ // none
          missGoLeft = distribution(random_number_generator) <= 0.5;
        } else if(imputation_method==1){
          if(newValueForMissing <= split_values[nodeID]){
            missGoLeft = true;
          }
         else{
           missGoLeft = false;
         }
        }
        ++missing_count;
      }
      
      //Random decision on missing values, count missing																					
      if (prediction_data->isOrderedVariable(split_varID)) {
        if ((missVal && missGoLeft) || ((!missVal) && value <= split_values[nodeID])) {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        } else {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      } else {
        size_t factorID = floor(value) - 1;
        size_t splitID = floor(split_values[nodeID]);

        // Left if 0 found at position factorID
        if ((missVal && missGoLeft) || ((!missVal) && !(splitID & (1ULL << factorID)))) {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        } else {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      }
    }
    prediction_terminal_nodeIDs[i] = nodeID;
	prediction_terminal_node_missing_counts[i] = missing_count;														   
  }
}

void Tree::computePermutationImportance(std::vector<double>& forest_importance, std::vector<double>& forest_variance,
    std::vector<double>& forest_importance_casewise) {

  size_t num_independent_variables = data->getNumCols();

  // Compute normal prediction accuracy for each tree. Predictions already computed..
  double accuracy_normal;
  std::vector<double> prederr_normal_casewise;
  std::vector<double> prederr_shuf_casewise;
  if (importance_mode == IMP_PERM_CASEWISE) {
    prederr_normal_casewise.resize(num_samples_oob, 0);
    prederr_shuf_casewise.resize(num_samples_oob, 0);
    accuracy_normal = computePredictionAccuracyInternal(&prederr_normal_casewise);
  } else {
    accuracy_normal = computePredictionAccuracyInternal(NULL);
  }

  prediction_terminal_nodeIDs.clear();
  prediction_terminal_nodeIDs.resize(num_samples_oob, 0);

  // Reserve space for permutations, initialize with oob_sampleIDs
  std::vector<size_t> permutations(oob_sampleIDs);

  // Randomly permute for all independent variables
  for (size_t i = 0; i < num_independent_variables; ++i) {

    // Permute and compute prediction accuracy again for this permutation and save difference
    permuteAndPredictOobSamples(i, permutations);
    double accuracy_permuted;
    if (importance_mode == IMP_PERM_CASEWISE) {
      accuracy_permuted = computePredictionAccuracyInternal(&prederr_shuf_casewise);
      for (size_t j = 0; j < num_samples_oob; ++j) {
        size_t pos = i * num_samples + oob_sampleIDs[j];
        forest_importance_casewise[pos] += prederr_shuf_casewise[j] - prederr_normal_casewise[j];
      }
    } else {
      accuracy_permuted = computePredictionAccuracyInternal(NULL);
    }

    double accuracy_difference = accuracy_normal - accuracy_permuted;
    forest_importance[i] += accuracy_difference;

    // Compute variance
    if (importance_mode == IMP_PERM_BREIMAN) {
      forest_variance[i] += accuracy_difference * accuracy_difference;
    } else if (importance_mode == IMP_PERM_LIAW) {
      forest_variance[i] += accuracy_difference * accuracy_difference * num_samples_oob;
    }
  }
}

// #nocov start
void Tree::appendToFile(std::ofstream& file) {

  // Save general fields
  saveVector2D(child_nodeIDs, file);
  saveVector1D(split_varIDs, file);
  saveVector1D(split_values, file);
  saveVector1D(imputed_values,file);

  // Call special functions for subclasses to save special fields.
  appendToFileInternal(file);
}
// #nocov end

void Tree::createPossibleSplitVarSubset(std::vector<size_t>& result) {

  size_t num_vars = data->getNumCols();

  // For corrected Gini importance add dummy variables
  if (importance_mode == IMP_GINI_CORRECTED) {
    num_vars += data->getNumCols();
  }

  // Randomly add non-deterministic variables (according to weights if needed)
  if (split_select_weights->empty()) {
    if (deterministic_varIDs->empty()) {
      drawWithoutReplacement(result, random_number_generator, num_vars, mtry);
    } else {
      drawWithoutReplacementSkip(result, random_number_generator, num_vars, (*deterministic_varIDs), mtry);
    }
  } else {
    drawWithoutReplacementWeighted(result, random_number_generator, num_vars, mtry, *split_select_weights);
  }

  // Always use deterministic variables
  std::copy(deterministic_varIDs->begin(), deterministic_varIDs->end(), std::inserter(result, result.end()));
}

bool Tree::splitNode(size_t nodeID) {

  std::uniform_real_distribution<double> distribution(0.0,1.0);														   
  // Select random subset of variables to possibly split at
  std::vector<size_t> possible_split_varIDs;
  createPossibleSplitVarSubset(possible_split_varIDs);

  // Call subclass method, sets split_varIDs and split_values
  bool stop = splitNodeInternal(nodeID, possible_split_varIDs);
  if (stop) {
    // Terminal node
    return true;
  }

  size_t split_varID = split_varIDs[nodeID];
  double split_value = split_values[nodeID];

  // Save non-permuted variable for prediction
  split_varIDs[nodeID] = data->getUnpermutedVarID(split_varID);

  // Create child nodes
  size_t left_child_nodeID = split_varIDs.size();
  child_nodeIDs[0][nodeID] = left_child_nodeID;
  createEmptyNode();
  start_pos[left_child_nodeID] = start_pos[nodeID];

  size_t right_child_nodeID = split_varIDs.size();
  child_nodeIDs[1][nodeID] = right_child_nodeID;
  createEmptyNode();
  start_pos[right_child_nodeID] = end_pos[nodeID];

  // For each sample in node, assign to left or right child
  size_t n_left=0;
  size_t n_right=0;
  double newValueForMissing;
  if(imputation_method == 1){
    // compute the median of non-missing values
    //newValueForMissing = data->get_median_of_non_missing(sampleIDs, split_varID, start_pos[nodeID], start_pos[right_child_nodeID]);
    newValueForMissing = imputed_values[nodeID];
  }
 
  // randomly pick a number btw 0, 1
  bool missGoLeft = false;
  bool missVal = false;			  
  if (data->isOrderedVariable(split_varID)) {
    // Ordered: left is <= splitval and right is > splitval
    size_t pos = start_pos[nodeID];
    while (pos < start_pos[right_child_nodeID]) {
      size_t sampleID = sampleIDs[pos];
	    missVal = std::isnan(data->get_x(sampleID, split_varID));
      if(missVal){
        if(imputation_method==0){ // none
          missGoLeft = distribution(random_number_generator) <= 0.5;
        } else if(imputation_method==1){
          missGoLeft = newValueForMissing <= split_value;
        }
        ++missing_count[sampleID];
      }
      // Randomly assign missing values to right or left child																		   									   
      if (((data->get_x(sampleID, split_varID) <= split_value)&&!missVal) || (missVal && missGoLeft)) {
        // If going to left, do nothing
        ++pos;
	    	++n_left;		 
      } else {
        // If going to right, move to right end
        --start_pos[right_child_nodeID];
        std::swap(sampleIDs[pos], sampleIDs[start_pos[right_child_nodeID]]);
	    	++n_right;		  
      }
    }
  } else {
    // Unordered: If bit at position is 1 -> right, 0 -> left
    size_t pos = start_pos[nodeID];
    while (pos < start_pos[right_child_nodeID]) {
      size_t sampleID = sampleIDs[pos];
      double level = data->get_x(sampleID, split_varID);
      size_t factorID = floor(level) - 1;
      size_t splitID = floor(split_value);

	  missVal = std::isnan(data->get_x(sampleID, split_varID));
      if(missVal){
        if(imputation_method==0){ // none
          missGoLeft = distribution(random_number_generator) <= 0.5;
        } else if(imputation_method==1){
          missGoLeft = newValueForMissing <= split_value;
        }
        ++missing_count[sampleID];
      }														   
      // Left if 0 found at position factorID
      if (((!(splitID & (1ULL << factorID)))&&!missVal)|| (missVal && missGoLeft)) {
        // If going to left, do nothing
        ++pos;
      } else {
        // If going to right, move to right end
        --start_pos[right_child_nodeID];
        std::swap(sampleIDs[pos], sampleIDs[start_pos[right_child_nodeID]]);
      }
    }
  }

  // End position of left child is start position of right child
  end_pos[left_child_nodeID] = start_pos[right_child_nodeID];
  end_pos[right_child_nodeID] = end_pos[nodeID];

  
  // No terminal node
  return false;
}

void Tree::createEmptyNode() {
  split_varIDs.push_back(0);
  split_values.push_back(0);
  imputed_values.push_back(0);
  child_nodeIDs[0].push_back(0);
  child_nodeIDs[1].push_back(0);
  start_pos.push_back(0);
  end_pos.push_back(0);

  createEmptyNodeInternal();
}

size_t Tree::dropDownSamplePermuted(size_t permuted_varID, size_t sampleID, size_t permuted_sampleID) {

  // Start in root and drop down
  size_t nodeID = 0;
  while (child_nodeIDs[0][nodeID] != 0 || child_nodeIDs[1][nodeID] != 0) {

    // Permute if variable is permutation variable
    size_t split_varID = split_varIDs[nodeID];
    size_t sampleID_final = sampleID;
    if (split_varID == permuted_varID) {
      sampleID_final = permuted_sampleID;
    }

    // Move to child
    double value = data->get_x(sampleID_final, split_varID);
    if (data->isOrderedVariable(split_varID)) {
      if (value <= split_values[nodeID]) {
        // Move to left child
        nodeID = child_nodeIDs[0][nodeID];
      } else {
        // Move to right child
        nodeID = child_nodeIDs[1][nodeID];
      }
    } else {
      size_t factorID = floor(value) - 1;
      size_t splitID = floor(split_values[nodeID]);

      // Left if 0 found at position factorID
      if (!(splitID & (1ULL << factorID))) {
        // Move to left child
        nodeID = child_nodeIDs[0][nodeID];
      } else {
        // Move to right child
        nodeID = child_nodeIDs[1][nodeID];
      }
    }

  }
  return nodeID;
}

void Tree::permuteAndPredictOobSamples(size_t permuted_varID, std::vector<size_t>& permutations) {

  // Permute OOB sample
  //std::vector<size_t> permutations(oob_sampleIDs);
  std::shuffle(permutations.begin(), permutations.end(), random_number_generator);

  // For each sample, drop down the tree and add prediction
  for (size_t i = 0; i < num_samples_oob; ++i) {
    size_t nodeID = dropDownSamplePermuted(permuted_varID, oob_sampleIDs[i], permutations[i]);
    prediction_terminal_nodeIDs[i] = nodeID;
  }
}

void Tree::bootstrap() {

  // Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * (*sample_fraction)[0];

  // Reserve space, reserve a little more to be save)
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp(-(*sample_fraction)[0]) + 0.1));

  std::uniform_int_distribution<size_t> unif_dist(0, num_samples - 1);

  // Start with all samples OOB
  inbag_counts.resize(num_samples, 0);

  // Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
  for (size_t s = 0; s < num_samples_inbag; ++s) {
    size_t draw = unif_dist(random_number_generator);
    sampleIDs.push_back(draw);
    ++inbag_counts[draw];
  }

  // Save OOB samples
  for (size_t s = 0; s < inbag_counts.size(); ++s) {
    if (inbag_counts[s] == 0) {
      oob_sampleIDs.push_back(s);
    }
  }
  num_samples_oob = oob_sampleIDs.size();

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }
}

void Tree::bootstrapWeighted() {

  // Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * (*sample_fraction)[0];

  // Reserve space, reserve a little more to be save)
  sampleIDs.reserve(num_samples_inbag);
  oob_sampleIDs.reserve(num_samples * (exp(-(*sample_fraction)[0]) + 0.1));

  std::discrete_distribution<> weighted_dist(case_weights->begin(), case_weights->end());

  // Start with all samples OOB
  inbag_counts.resize(num_samples, 0);

  // Draw num_samples samples with replacement (n out of n) as inbag and mark as not OOB
  for (size_t s = 0; s < num_samples_inbag; ++s) {
    size_t draw = weighted_dist(random_number_generator);
    sampleIDs.push_back(draw);
    ++inbag_counts[draw];
  }

  // Save OOB samples. In holdout mode these are the cases with 0 weight.
  if (holdout) {
    for (size_t s = 0; s < (*case_weights).size(); ++s) {
      if ((*case_weights)[s] == 0) {
        oob_sampleIDs.push_back(s);
      }
    }
  } else {
    for (size_t s = 0; s < inbag_counts.size(); ++s) {
      if (inbag_counts[s] == 0) {
        oob_sampleIDs.push_back(s);
      }
    }
  }
  num_samples_oob = oob_sampleIDs.size();

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }
}

void Tree::bootstrapWithoutReplacement() {

  // Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * (*sample_fraction)[0];
  shuffleAndSplit(sampleIDs, oob_sampleIDs, num_samples, num_samples_inbag, random_number_generator);
  num_samples_oob = oob_sampleIDs.size();

  if (keep_inbag) {
    // All observation are 0 or 1 times inbag
    inbag_counts.resize(num_samples, 1);
    for (size_t i = 0; i < oob_sampleIDs.size(); i++) {
      inbag_counts[oob_sampleIDs[i]] = 0;
    }
  }
}

void Tree::bootstrapWithoutReplacementWeighted() {

  // Use fraction (default 63.21%) of the samples
  size_t num_samples_inbag = (size_t) num_samples * (*sample_fraction)[0];
  drawWithoutReplacementWeighted(sampleIDs, random_number_generator, num_samples - 1, num_samples_inbag, *case_weights);

  // All observation are 0 or 1 times inbag
  inbag_counts.resize(num_samples, 0);
  for (auto& sampleID : sampleIDs) {
    inbag_counts[sampleID] = 1;
  }

  // Save OOB samples. In holdout mode these are the cases with 0 weight.
  if (holdout) {
    for (size_t s = 0; s < (*case_weights).size(); ++s) {
      if ((*case_weights)[s] == 0) {
        oob_sampleIDs.push_back(s);
      }
    }
  } else {
    for (size_t s = 0; s < inbag_counts.size(); ++s) {
      if (inbag_counts[s] == 0) {
        oob_sampleIDs.push_back(s);
      }
    }
  }
  num_samples_oob = oob_sampleIDs.size();

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }
}

void Tree::bootstrapClassWise() {
  // Empty on purpose (virtual function only implemented in classification and probability)
}

void Tree::bootstrapWithoutReplacementClassWise() {
  // Empty on purpose (virtual function only implemented in classification and probability)
}

void Tree::setManualInbag() {
  // Select observation as specified in manual_inbag vector
  sampleIDs.reserve(manual_inbag->size());
  inbag_counts.resize(num_samples, 0);
  for (size_t i = 0; i < manual_inbag->size(); ++i) {
    size_t inbag_count = (*manual_inbag)[i];
    if ((*manual_inbag)[i] > 0) {
      for (size_t j = 0; j < inbag_count; ++j) {
        sampleIDs.push_back(i);
      }
      inbag_counts[i] = inbag_count;
    } else {
      oob_sampleIDs.push_back(i);
    }
  }
  num_samples_oob = oob_sampleIDs.size();

  // Shuffle samples
  std::shuffle(sampleIDs.begin(), sampleIDs.end(), random_number_generator);

  if (!keep_inbag) {
    inbag_counts.clear();
    inbag_counts.shrink_to_fit();
  }
}

} // namespace ranger
