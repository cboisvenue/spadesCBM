#include <Rcpp.h>
#include <string> 
#include <sstream>
#include <iostream>
#include <unordered_map>
#include <vector>


template < typename T > std::string to_string( const T& n )
{
  std::ostringstream stm ;
  stm << n ;
  return stm.str() ;
}

/**
* square sparse matrix specifically purposed for a vector by matrix
* multiplication, and the flux operation: @f$ flux=diag(v_{0})\times(M-I) @f$
*
* Implemented with coordinate format for off-diagonal values. Diagonal values
* are stored as an array.
*/
class Coomatrix
{
private:
  
  /**
  * identifier of this matrix
  */
  int Id;
  
  /**
  * The coordinate storage for off-diagonal row indices
  */
  int* RowIndices;
  
  /**
  * The coordinate storage for off-diagonal column indices
  */
  int* ColIndices;
  
  /**
  * The values for each row and off-diagonal column index
  */
  double* Values;
  
  /**
  * The diagonal values (of constant length Order)
  */
  double* Diagonal;
  
  /**
  * the n by n square dimension of the matrix
  */
  int Order;
  
  /**
  * The allocated size of the row, col, and value arrays. This may be greater
  * than or equal to the number of defined values specified by Count.
  */
  int AllocatedSize;
  
  /**
  * the number of defined off-diagonal entries in the matrix RowIndices,
  * ColIndices, and Values arrays
  */
  int Count;
  
  /**
  * allocate additional memory to store values by doubling the allocated size
  */
  void Resize(){
    int _size = AllocatedSize * 2;
    int* _rowIndices = new int[_size];
    int* _colIndices = new int[_size];
    double* _values = new double[_size];
    for (int i = 0; i < AllocatedSize; i++)
    {
      _rowIndices[i] = RowIndices[i];
      _colIndices[i] = ColIndices[i];
      _values[i] = Values[i];
    }
    delete[] RowIndices;
    delete[] ColIndices;
    delete[] Values;
    RowIndices = _rowIndices;
    ColIndices = _colIndices;
    Values = _values;
    AllocatedSize = _size;
  }
  
  
public:
  /**
  * Gets the matrix id
  * @return the matrix id
  */
  int GetId() { return Id; }
  
  /**
  * Initialize a new coordinate matrix with specified identifier, and order
  * @param id the identifier for the matrix
  * @param order the number of rows, and columns in the matrix
  */
  Coomatrix(int id, int order) {
    size_t coomatrixInitialSize = 64;
    if (order <= 0)
    {
      throw std::invalid_argument("order must be > 0");
    }
    
    Id = id;
    AllocatedSize = coomatrixInitialSize;
    RowIndices = new int[AllocatedSize];
    ColIndices = new int[AllocatedSize];
    Values = new double[AllocatedSize];
    Diagonal = new double[order];
    Count = 0;
    for (int i = 0; i < order; i++)
    {
      Diagonal[i] = 0;
    }
    Order = order;
  }
  
  /**
  * Destroy the coordinate matrix, all allocated memory is freed
  */
  ~Coomatrix() {
    delete[] RowIndices;
    delete[] ColIndices;
    delete[] Values;
    delete[] Diagonal;
  }
  /**
  * Reset the matrix by setting the Count to 0 and the diagonal values to 0
  */
  void Reset() {
    Count = 0;
  }
  
  /**
  * Add an entry to the matrix.
  * @param rowIndex the 0 based row position of the value within the matrix
  * @param colIndex the 0 based col position of the value within the matrix
  * @param value the value at the specified row/column pair
  */
  void AddValue(int rowIndex, int colIndex, double value){
    if (rowIndex == colIndex)
    {
      Diagonal[rowIndex] = value;
    }
    else
    {
      if (value == 0)
      {
        //no need to store zero values in the off-diagonals since
        //all off-diagonals are assumed to be zero unless they are set
        return;
      }
      //check if the storage is full
      if (Count == AllocatedSize)
      {
        //create space for more triplets
        Resize();
      }
      RowIndices[Count] = rowIndex;
      ColIndices[Count] = colIndex;
      Values[Count] = value;
      Count++;
    }
  }
  
  /**
  * Initialize this matrix with values and a default diagonal init value.
  * If the
  * @param init the value to assign to each diagonal element not specified in
  *        the other parameters
  * @param num the number of elements in the rowIndices, colIndicies, and
  *        values arrays
  * @param rowIndices an array of length num where each value corresponds
  *        to a row in the matrix
  * @param colIndices an array of length num where each value corresponds
  *        to a column in the matrix
  * @param values an array of length num where each value at position i
  *        corresponds to a value in the matrix at rowIndices[i],
  *        colIndices[i]
  */
  void DiagonalInit(double init, int num, int* rowIndices, int* colIndices,
                    double* values){
    Reset();
    for (int i = 0; i < Order; i++)
    {
      Diagonal[i] = init;
    }
    for (int i = 0; i < num; i++)
    {
      AddValue(rowIndices[i], colIndices[i], values[i]);
    }
  }
  
  /**
  * Initialize this matrix with values.
  * @param init the value to assign to each diagonal element not specified in
  *        the other parameters
  * @param num the number of elements in the rowIndices, colIndicies, and
  *        values arrays
  * @param rowIndices an array of length num where each value corresponds
  *        to a row in the matrix
  * @param colIndices an array of length num where each value corresponds
  *        to a column in the matrix
  * @param values an array of length num where each value at position i
  *        corresponds to a value in the matrix at rowIndices[i],
  *        colIndices[i]
  */
  void Init(int num, int* rowIndices, int* colIndices, double* values){
    Reset();
    for (int i = 0; i < Order; i++)
    {
      Diagonal[i] = 0.0;
    }
    for (int i = 0; i < num; i++)
    {
      AddValue(rowIndices[i], colIndices[i], values[i]);
    }
  }
  
  /**
  * Get the defined values in this matrix as coordinate and value arrays.  The
  * number of defined values the return value.  The rows columns and values
  * are returned by reference overwriting any data in the supplied std::vector
  * objects
  * @param rows the sequential row coordinates where each ordered item
  *        corresponds to the row position of the value in the matrix
  * @param cols the sequential column coordinates where each ordered item
  *        corresponds to the column position of the value in the matrix
  * @param values the sequential values at each row/column pair
  */
  int GetValues(std::vector<int>& rows, std::vector<int>& cols, std::vector<double>& values){
    int size = Order + Count;
    rows.clear();//clear just in case
    cols.clear();
    values.clear();
    for (int i = 0; i < Order; i++)
    {
      rows.push_back(i);
      cols.push_back(i);
      values.push_back(Diagonal[i]);
    }
    for (int i = 0; i < Count; i++)
    {
      rows.push_back(RowIndices[i]);
      cols.push_back(ColIndices[i]);
      values.push_back(Values[i]);
    }
    return size;
  }
  
  /**
  * compute the vector by matrix operation @f$ v_1 = v_0 \times M @f$
  * v1 and v0 must be of length Order as specified in this instance
  * @param v0 the vector left hand operand of the function
  * @param v1 the return value of the function, any data in this array is
  *        overwritten
  */
  void Compute(double* v0, double* v1){
    memset(v1, 0, Order*sizeof(double));
    
    for (int i = 0; i < Order; i++)
    {
      v1[i] += v0[i] * Diagonal[i];
    }
    for (int index = 0; index < Count; index++)
    {
      v1[ColIndices[index]] += v0[RowIndices[index]] * Values[index];
    }
  }
  
  /**
  * compute the flux operation:
  *
  * @f$ flux=diag(v_0)\times(M-I) @f$
  *
  * and also the vector matrix product:
  * @f$ v_1 = v_0 \times M @f$
  *
  * v1 and v0 must be of length Order as specified in this instance
  * M must also be of same Order
  * this instance stores flux
  *
  * @param v0 the vector left hand operand of the function
  * @param v1 the return value of the function, any data in this array is
  *        overwritten
  * @param M the matrix of proportional transfers from v0 to v1
  */
  void Flux(double* v0, double* v1, Coomatrix* M){
    //reset this matrix to zero
    Reset();
    
    memset(v1, 0, Order*sizeof(double));
    
    for (int i = 0; i < M->Count; i++)
    {
      int col = M->ColIndices[i];
      int row = M->RowIndices[i];
      double value = M->Values[i];
      double t = v0[row] * value;
      AddValue(row, col, t);
      v1[col] += t;
    }
    
    for (int i = 0; i < Order; i++)
    {
      v1[i] += v0[i] * M->Diagonal[i];
      AddValue(i, i, v0[i] * (M->Diagonal[i] - 1));
    }
  }
};
void FillMatrix(int maxIndex, Coomatrix& coomat, Rcpp::NumericMatrix& mat){
  for( auto matRow = 0; matRow<mat.nrow(); matRow++){
    int rowIdx = (int)mat(matRow, 0)-1;
    int colIdx = (int)mat(matRow, 1)-1;
    double value = mat(matRow, 2);
    if(rowIdx < 0 || rowIdx >= maxIndex ||
       colIdx < 0 || colIdx >= maxIndex){
      throw std::invalid_argument("matrix index out of range");
    }
    coomat.AddValue(rowIdx, colIdx, value);
  }
}


// [[Rcpp::export]]
void StepPoolsRef(Rcpp::NumericMatrix& pools, Rcpp::IntegerMatrix& opMatrix, 
               Rcpp::List& flowMatrices) {
  
  auto npools = pools.ncol();
  auto nstands = pools.nrow();
  
  
  //check bounds of opMatrix row (should always match nstands)
  if(nstands != opMatrix.nrow()){
    throw std::invalid_argument("number of rows in pools and number of opMatrix rows do not match");
  }
  
  std::vector<double> workingPools(npools,0.0);
  //iterate over the opMatrix performing the dynamics
  Coomatrix _mat(0,npools);
  for(auto row = 0; row < opMatrix.nrow(); row ++){
    //get the current pools for the row
    Rcpp::NumericVector _currentPools = pools(row, Rcpp::_);
    std::vector<double> currentPools(_currentPools.begin(), _currentPools.end());
    
    for(auto col = 0; col < opMatrix.ncol(); col ++ ){
      size_t id = opMatrix(row,col);
      if(id<=0){
        continue;
      }
      //fetch the pre-allocated matrix corresponding to the id in the op matrix
      SEXP flowCol = flowMatrices[col];
      //Rcpp::NumericMatrix mat;
      if(TYPEOF(flowCol) == ENVSXP )
      {
        Rcpp::Environment hash = (Rcpp::Environment)flowCol;
        Rcpp::NumericMatrix mat = hash[to_string(id)];
        FillMatrix(npools, _mat, mat);
      }
      else 
      { //assume list
        Rcpp::List list = (Rcpp::List)flowCol;
        
        Rcpp::NumericMatrix mat = list[id-1];
        FillMatrix(npools, _mat, mat);
      }
      
      _mat.Compute(currentPools.data(), workingPools.data());
      currentPools = workingPools;
      _mat.Reset();
    }
    
    Rcpp::NumericVector res(currentPools.begin(), currentPools.end());
    
    pools(row, Rcpp::_) = res;
  }
}
// [[Rcpp::export]]
Rcpp::NumericMatrix StepPools(Rcpp::NumericMatrix& pools, Rcpp::IntegerMatrix& opMatrix, 
                              Rcpp::List& flowMatrices){
  Rcpp::NumericMatrix poolsClone(clone(pools));
  StepPoolsRef(poolsClone, opMatrix, flowMatrices);
  return poolsClone;
}
//// [[Rcpp::export]]
//Rcpp::List StepFlux(Rcpp::List& data, Rcpp::IntegerMatrix& opMatrix, 
//    Rcpp::List& flowMatrices, Rcpp::IntegerMatrix& fluxIndicators) {
//
//  TODO
//}

struct AGBiomassIncrement {
	AGBiomassIncrement() {
		SWM = 0;
		SWF = 0;
		SWO = 0;
		HWM = 0;
		HWF = 0;
		HWO = 0;
	}
	double SWM;
	double SWF;
	double SWO;
	double HWM;
	double HWF;
	double HWO;
};

struct TotalBiomassIncrement {
	TotalBiomassIncrement(double swm, double swf, double swo, double swfr, 
		double swcr, double hwm, double hwf, double hwo, double hwfr, 
		double hwcr) {
		SWM = swm;
		SWF = swf;
		SWO = swo;
		SWFR = swfr;
		SWCR = swcr;
		HWM = hwm;
		HWF = hwf;
		HWO = hwo;
		HWFR = hwfr;
		HWCR = hwcr;
	}
	double SWM;
	double SWF;
	double SWO;
	double SWCR;
	double SWFR;
	double HWM;
	double HWF;
	double HWO;
	double HWCR;
	double HWFR;
};

struct RootParameter {
	double rb_hw_a;
	double rb_sw_a;
	double rb_hw_b;
	double frp_a;
	double frp_b;
	double frp_c;
};

struct PoolNames {
	PoolNames() {
		NPools = 26;

		Rcpp::Environment globalEnv = Rcpp::Environment::global_env();

		//set up the pool indices
		Input = (int)globalEnv["Input"] - 1;
		SoftwoodMerch = (int)globalEnv["SoftwoodMerch"] - 1;
		SoftwoodFoliage = (int)globalEnv["SoftwoodFoliage"] - 1;
		SoftwoodOther = (int)globalEnv["SoftwoodOther"] - 1;
		SoftwoodCoarseRoots = (int)globalEnv["SoftwoodCoarseRoots"] - 1;
		SoftwoodFineRoots = (int)globalEnv["SoftwoodFineRoots"] - 1;
		HardwoodMerch = (int)globalEnv["HardwoodMerch"] - 1;
		HardwoodFoliage = (int)globalEnv["HardwoodFoliage"] - 1;
		HardwoodOther = (int)globalEnv["HardwoodOther"] - 1;
		HardwoodCoarseRoots = (int)globalEnv["HardwoodCoarseRoots"] - 1;
		HardwoodFineRoots = (int)globalEnv["HardwoodFineRoots"] - 1;
		AboveGroundVeryFastSoil = (int)globalEnv["AboveGroundVeryFastSoil"] - 1;
		BelowGroundVeryFastSoil = (int)globalEnv["BelowGroundVeryFastSoil"] - 1;
		AboveGroundFastSoil = (int)globalEnv["AboveGroundFastSoil"] - 1;
		BelowGroundFastSoil = (int)globalEnv["BelowGroundFastSoil"] - 1;
		MediumSoil = (int)globalEnv["MediumSoil"] - 1;
		AboveGroundSlowSoil = (int)globalEnv["AboveGroundSlowSoil"] - 1;
		BelowGroundSlowSoil = (int)globalEnv["BelowGroundSlowSoil"] - 1;
		SoftwoodStemSnag = (int)globalEnv["SoftwoodStemSnag"] - 1;
		SoftwoodBranchSnag = (int)globalEnv["SoftwoodBranchSnag"] - 1;
		HardwoodStemSnag = (int)globalEnv["HardwoodStemSnag"] - 1;
		HardwoodBranchSnag = (int)globalEnv["HardwoodBranchSnag"] - 1;
		CO2 = (int)globalEnv["CO2"] - 1;
		CH4 = (int)globalEnv["CH4"] - 1;
		CO = (int)globalEnv["CO"] - 1;
		Products = (int)globalEnv["Products"] - 1;
	}
	int NPools;
	int Input;
	int SoftwoodMerch;
	int SoftwoodFoliage;
	int SoftwoodOther;
	int SoftwoodCoarseRoots;
	int SoftwoodFineRoots;
	int HardwoodMerch;
	int HardwoodFoliage;
	int HardwoodOther;
	int HardwoodCoarseRoots;
	int HardwoodFineRoots;
	int AboveGroundVeryFastSoil;
	int BelowGroundVeryFastSoil;
	int AboveGroundFastSoil;
	int BelowGroundFastSoil;
	int MediumSoil;
	int AboveGroundSlowSoil;
	int BelowGroundSlowSoil;
	int SoftwoodStemSnag;
	int SoftwoodBranchSnag;
	int HardwoodStemSnag;
	int HardwoodBranchSnag;
	int CO2;
	int CH4;
	int CO;
	int Products;
};

TotalBiomassIncrement GetTotalBiomassIncrement(PoolNames& pn, double* pools,
	AGBiomassIncrement inc, RootParameter rootParam,
	double biomassToCarbonRate, double sw_multiplier,
	double hw_multiplier) {


	double rb_hw_a = rootParam.rb_hw_a;
	double rb_sw_a = rootParam.rb_sw_a;
	double rb_hw_b = rootParam.rb_hw_b;
	double frp_a = rootParam.frp_a;
	double frp_b = rootParam.frp_b;
	double frp_c = rootParam.frp_c;

	//clamp the increment so it doesnt cause the pools to go negative
	inc.SWM = std::max(-pools[pn.SoftwoodMerch], inc.SWM);
	inc.SWF = std::max(-pools[pn.SoftwoodFoliage], inc.SWF);
	inc.SWO = std::max(-pools[pn.SoftwoodOther], inc.SWO);
	inc.HWM = std::max(-pools[pn.HardwoodMerch], inc.HWM);
	inc.HWF = std::max(-pools[pn.HardwoodFoliage], inc.HWF);
	inc.HWO = std::max(-pools[pn.HardwoodOther], inc.HWO);

	double totalHWAgBioC =
		pools[pn.HardwoodMerch] + inc.HWM +
		pools[pn.HardwoodFoliage] + inc.HWF +
		pools[pn.HardwoodOther] + inc.HWO;

	double totalSWAgBioC =
		pools[pn.SoftwoodMerch] + inc.SWM +
		pools[pn.SoftwoodFoliage] + inc.SWF +
		pools[pn.SoftwoodOther] + inc.SWO;

	double totalRootBioHW = rb_hw_a *
		pow(totalHWAgBioC / biomassToCarbonRate, rb_hw_b);
	double totalRootBioSW = rb_sw_a * totalSWAgBioC
		/ biomassToCarbonRate;
	double fineRootPortion = frp_a + frp_b *
		exp(frp_c * (totalRootBioHW + totalRootBioSW));

	double swcr_inc = totalRootBioSW * (1 - fineRootPortion) *
		biomassToCarbonRate - pools[pn.SoftwoodCoarseRoots];
	double swfr_inc = totalRootBioSW * fineRootPortion *
		biomassToCarbonRate - pools[pn.SoftwoodFineRoots];
	double hwcr_inc = totalRootBioHW * (1 - fineRootPortion) *
		biomassToCarbonRate - pools[pn.HardwoodCoarseRoots];
	double hwfr_inc = totalRootBioHW * fineRootPortion *
		biomassToCarbonRate - pools[pn.HardwoodFineRoots];

	TotalBiomassIncrement total_increment(
		inc.SWM * sw_multiplier,
		inc.SWF * sw_multiplier,
		inc.SWO * sw_multiplier,
		swfr_inc * sw_multiplier,
		swcr_inc * sw_multiplier,
		inc.HWM * hw_multiplier,
		inc.HWF * hw_multiplier,
		inc.HWO * hw_multiplier,
		hwfr_inc * hw_multiplier,
		hwcr_inc * hw_multiplier);
	return total_increment;
}


//' compute flows to dom pools that occur on a growth curve decline
// [[Rcpp::export]]
Rcpp::NumericMatrix ComputeOvermatureDecline(Rcpp::NumericMatrix growthIncrements,
	Rcpp::DataFrame& turnoverParams) {

	double OtherToBranchSnagSplit = turnoverParams["OtherToBranchSnagSplit"];
	double CoarseRootAGSplit = turnoverParams["CoarseRootAGSplit"];
	double FineRootAGSplit = turnoverParams["FineRootAGSplit"];

	Rcpp::NumericMatrix overmatureDecline(growthIncrements.nrow(), 9);
	for (R_len_t i = 0; i < growthIncrements.nrow(); i++) {
		R_len_t id = growthIncrements(i, 0);
		TotalBiomassIncrement inc(
			growthIncrements(i, 1),
			growthIncrements(i, 2),
			growthIncrements(i, 3),
			growthIncrements(i, 4),
			growthIncrements(i, 5),
			growthIncrements(i, 6),
			growthIncrements(i, 7),
			growthIncrements(i, 8),
			growthIncrements(i, 9),
			growthIncrements(i, 10));

		double totalIncrementHW = inc.HWM + inc.HWO + inc.HWF
			+ inc.HWFR + inc.HWCR;

		double totalIncrementSW = inc.SWM + inc.SWO + inc.SWF
			+ inc.SWFR + inc.SWCR;

		double HWMerchToStemSnags = 0;
		double SWMerchToStemSnags = 0;
		double HWOtherToBranchSnag = 0;
		double SWOtherToBranchSnag = 0;
		double HWOtherToAGFast = 0;
		double SWOtherToAGFast = 0;
		double HWCoarseRootToAGFast = 0;
		double SWCoarseRootToAGFast = 0;
		double HWCoarseRootToBGFast = 0;
		double SWCoarseRootToBGFast = 0;
		double HWFoliageToAGVeryFast = 0;
		double HWFineRootToAGVeryFast = 0;
		double SWFoliageToAGVeryFast = 0;
		double SWFineRootToAGVeryFast = 0;
		double HWFineRootToBGVeryFast = 0;
		double SWFineRootToBGVeryFast = 0;

		if (totalIncrementSW < 0)
		{
			if (inc.SWM < 0)
			{
				SWMerchToStemSnags = -inc.SWM;
			}
			if (inc.SWF < 0)
			{
				SWFoliageToAGVeryFast = -inc.SWF;
			}
			if (inc.SWO < 0)
			{
				SWOtherToBranchSnag = -inc.SWO * OtherToBranchSnagSplit;
				SWOtherToAGFast = -inc.SWO * (1 - OtherToBranchSnagSplit);
			}
			if (inc.SWCR < 0)
			{
				SWCoarseRootToAGFast = -inc.SWCR * CoarseRootAGSplit;
				SWCoarseRootToBGFast = -inc.SWCR * (1 - CoarseRootAGSplit);
			}
			if (inc.SWFR < 0)
			{
				SWFineRootToAGVeryFast = -inc.SWFR * FineRootAGSplit;
				SWFineRootToBGVeryFast = -inc.SWFR * (1 - FineRootAGSplit);
			}
		}

		if (totalIncrementHW < 0)
		{
			if (inc.HWM < 0)
			{
				HWMerchToStemSnags = -inc.HWM;
			}
			if (inc.HWF < 0)
			{
				HWFoliageToAGVeryFast = -inc.HWF;
			}
			if (inc.HWO < 0)
			{
				HWOtherToBranchSnag = -inc.HWO * OtherToBranchSnagSplit;
				HWOtherToAGFast = -inc.HWO * (1 - OtherToBranchSnagSplit);
			}
			if (inc.HWCR < 0)
			{
				HWCoarseRootToAGFast = -inc.HWCR * CoarseRootAGSplit;
				HWCoarseRootToBGFast = -inc.HWCR * (1 - CoarseRootAGSplit);
			}
			if (inc.HWFR < 0)
			{
				HWFineRootToAGVeryFast = -inc.HWFR * FineRootAGSplit;
				HWFineRootToBGVeryFast = -inc.HWFR * (1 - FineRootAGSplit);
			}
		}
		double ToAGfast = HWOtherToAGFast + SWOtherToAGFast + HWCoarseRootToAGFast + SWCoarseRootToAGFast;
		double ToBGFast = HWCoarseRootToBGFast + SWCoarseRootToBGFast;
		double ToAGVFast = HWFoliageToAGVeryFast + HWFineRootToAGVeryFast + SWFoliageToAGVeryFast + SWFineRootToAGVeryFast;
		double ToBGVFast = HWFineRootToBGVeryFast + SWFineRootToBGVeryFast;

		overmatureDecline(i, 0) = id;
		overmatureDecline(i, 1) = SWMerchToStemSnags;
		overmatureDecline(i, 2) = HWMerchToStemSnags;
		overmatureDecline(i, 3) = SWOtherToBranchSnag;
		overmatureDecline(i, 4) = HWOtherToBranchSnag;
		overmatureDecline(i, 5) = ToAGfast;
		overmatureDecline(i, 6) = ToBGFast;
		overmatureDecline(i, 7) = ToAGVFast;
		overmatureDecline(i, 8) = ToBGVFast;
	}
	colnames(overmatureDecline) = Rcpp::CharacterVector::create(
		"Id", "SWStemSnags", "HWStemSnags", "SWBranchSnags", "HWBranchSnags", "AGFast",
		"BGFast", "AGVFast", "BGVFast");
	return overmatureDecline;
}

//' compute the total growth (above ground and belowground) increment matrices
//' for all stands based on age and hashtable of growth increments by growth
//' curve id
// [[Rcpp::export]]
Rcpp::NumericMatrix ComputeGrowthIncrements(Rcpp::Environment& growthIncrements,
	Rcpp::IntegerVector& ages, Rcpp::IntegerVector& gcids,
	Rcpp::NumericMatrix& pools, Rcpp::DataFrame& rootParameters,
	double biomassToCarbonRate, double swMult = 1.0, double hwMult = 1.0) {

	PoolNames pn;
	Rcpp::NumericMatrix totalIncrements(gcids.length(), 11);

	AGBiomassIncrement inc;
	Rcpp::NumericMatrix agIncrements(gcids.length(), 6);
	std::fill(agIncrements.begin(), agIncrements.end(), 0.0);
	for (R_len_t i = 0; i < gcids.length(); i++) {
		int gcid = gcids[i];
		int age = ages[i];

		SEXP ageHash = growthIncrements[to_string(gcid)];
		if (ageHash == R_NilValue) {
			throw std::invalid_argument("specified gcid does not exist");
		}
		SEXP agInc = ((Rcpp::Environment)ageHash)[to_string(age)];

		if (agInc != R_NilValue) {
			Rcpp::NumericVector R_increments = Rcpp::NumericVector(agInc);
			inc.SWM = R_increments[0];
			inc.SWF = R_increments[1];
			inc.SWO = R_increments[2];
			inc.HWM = R_increments[3];
			inc.HWF = R_increments[4];
			inc.HWO = R_increments[5];
		}

		Rcpp::NumericVector poolRow = pools(i, Rcpp::_);
		std::vector<double> _pools(poolRow.begin(), poolRow.end());

		RootParameter rp;
		rp.rb_hw_a = rootParameters["rb_hw_a"];
		rp.rb_sw_a = rootParameters["rb_sw_a"];
		rp.rb_hw_b = rootParameters["rb_hw_b"];
		rp.frp_a = rootParameters["frp_a"];
		rp.frp_b = rootParameters["frp_b"];
		rp.frp_c = rootParameters["frp_c"];
		TotalBiomassIncrement totalInc = GetTotalBiomassIncrement(pn,
			_pools.data(), inc, rp, biomassToCarbonRate, swMult, hwMult);

		totalIncrements(i, 0) = i + 1; //stand index
		totalIncrements(i, 1) = totalInc.SWM;
		totalIncrements(i, 2) = totalInc.SWF;
		totalIncrements(i, 3) = totalInc.SWO;
		totalIncrements(i, 4) = totalInc.SWFR;
		totalIncrements(i, 5) = totalInc.SWCR;
		totalIncrements(i, 6) = totalInc.HWM;
		totalIncrements(i, 7) = totalInc.HWF;
		totalIncrements(i, 8) = totalInc.HWO;
		totalIncrements(i, 9) = totalInc.HWFR;
		totalIncrements(i, 10) = totalInc.HWCR;
	}
	colnames(totalIncrements) = Rcpp::CharacterVector::create(
		"Id", "SWMerch", "SWFoliage", "SWOther", "SWFineRoot", "SWCoarseRoot",
		"HWMerch", "HWFoliage", "HWOther", "HWFineRoot", "HWCoarseRoot");
	return totalIncrements;

}

//' transforms growth matrix into coordinate format matrix in terms of CBM pools
Rcpp::List ComputeGrowthCoordinateMatrices(PoolNames& pn,
	Rcpp::NumericMatrix& growthIncrements) {

	Rcpp::List matrices(growthIncrements.nrow());

	for (R_len_t i = 0; i < growthIncrements.nrow(); i++) {
		std::vector<Rcpp::NumericVector> incrementRows;
		R_len_t id = growthIncrements(i, 0);
		TotalBiomassIncrement total_increment(
			growthIncrements(i, 1),
			growthIncrements(i, 2),
			growthIncrements(i, 3),
			growthIncrements(i, 4),
			growthIncrements(i, 5),
			growthIncrements(i, 6),
			growthIncrements(i, 7),
			growthIncrements(i, 8),
			growthIncrements(i, 9),
			growthIncrements(i, 10));

		if (total_increment.SWM != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodMerch + 1, total_increment.SWM));
		if (total_increment.SWF != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodFoliage + 1, total_increment.SWF));
		if (total_increment.SWO != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodOther + 1, total_increment.SWO));
		if (total_increment.SWFR != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodFineRoots + 1, total_increment.SWFR));
		if (total_increment.SWCR != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodCoarseRoots + 1, total_increment.SWCR));

		if (total_increment.HWM != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodMerch + 1, total_increment.HWM));
		if (total_increment.HWF != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodFoliage + 1, total_increment.HWF));
		if (total_increment.HWO != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodOther + 1, total_increment.HWO));
		if (total_increment.HWFR != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodFineRoots + 1, total_increment.HWFR));
		if (total_increment.HWCR != 0) incrementRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodCoarseRoots + 1, total_increment.HWCR));

		for (R_len_t poolidx = 0; poolidx < pn.NPools; poolidx++) {
			incrementRows.push_back(Rcpp::NumericVector::create(poolidx + 1, poolidx + 1, 1.0));
		}
		Rcpp::NumericMatrix result(incrementRows.size(), 3);
		for (R_len_t rowidx = 0; rowidx < incrementRows.size(); rowidx++) {
			result(rowidx, Rcpp::_) = incrementRows[rowidx];
		}
		colnames(result) = Rcpp::CharacterVector::create("row", "col", "value");
		matrices[i] = result;
	}

	return matrices;
}

//' transforms overmature decline matrix into coordinate format matrix in terms
//' of CBM pools
Rcpp::List ComputeOverMatureDeclineCoordinateMatrices(PoolNames& pn,
	Rcpp::NumericMatrix& decline) {

	Rcpp::List matrices(decline.nrow());

	for (R_len_t i = 0; i < decline.nrow(); i++) {
		std::vector<Rcpp::NumericVector> declineRows;
		R_len_t id = decline(i, 0);
		double SWStemSnags = decline(i, 1);
		double HWStemSnags = decline(i, 2);
		double SWBranchSnags = decline(i, 3);
		double HWBranchSnags = decline(i, 4);
		double AGFast = decline(i, 5);
		double BGFast = decline(i, 6);
		double AGVFast = decline(i, 7);
		double BGVFast = decline(i, 8);

		if (SWStemSnags != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodStemSnag + 1, SWStemSnags));
		if (HWStemSnags != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodStemSnag + 1, HWStemSnags));
		if (SWBranchSnags != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.SoftwoodBranchSnag + 1, SWBranchSnags));
		if (HWBranchSnags != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.HardwoodBranchSnag + 1, HWBranchSnags));

		if (AGFast != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.AboveGroundFastSoil + 1, AGFast));
		if (BGFast != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.BelowGroundFastSoil + 1, BGFast));
		if (AGVFast != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.AboveGroundVeryFastSoil + 1, AGVFast));
		if (BGVFast != 0) declineRows.push_back(Rcpp::NumericVector::create(pn.Input + 1, pn.BelowGroundVeryFastSoil + 1, BGVFast));

		for (R_len_t poolidx = 0; poolidx < pn.NPools; poolidx++) {
			declineRows.push_back(Rcpp::NumericVector::create(poolidx + 1, poolidx + 1, 1.0));
		}

		Rcpp::NumericMatrix result(declineRows.size(), 3);
		for (R_len_t rowidx = 0; rowidx < declineRows.size(); rowidx++) {
			result(rowidx, Rcpp::_) = declineRows[rowidx];
		}
		colnames(result) = Rcpp::CharacterVector::create("row", "col", "value");
		matrices[i] = result;
	}

	return matrices;
}

// [[Rcpp::export]]
Rcpp::List ComputeGrowthAndDeclineMatrices(Rcpp::NumericMatrix& growthIncrements,
	Rcpp::NumericMatrix& decline) {
	PoolNames pn;

	return Rcpp::List::create(
		Rcpp::Named("Growth") = ComputeGrowthCoordinateMatrices(pn, growthIncrements),
		Rcpp::Named("OvermatureDecline") = ComputeOverMatureDeclineCoordinateMatrices(pn, decline)
		);
}

// [[Rcpp::export]]
Rcpp::List ComputeGrowthAndDeclineMatrices2(Rcpp::Environment& growthIncrements,
	Rcpp::IntegerVector& ages, Rcpp::IntegerVector& gcids,
	Rcpp::NumericMatrix& pools, Rcpp::DataFrame& rootParameters,
	Rcpp::DataFrame& turnoverParams, double biomassToCarbonRate,
	double swMult = 1.0, double hwMult = 1.0) {

	PoolNames pn;
	Rcpp::NumericMatrix inc = ComputeGrowthIncrements(
		growthIncrements, ages, gcids, pools, rootParameters,
		biomassToCarbonRate, swMult, hwMult);

	Rcpp::NumericMatrix decline = ComputeOvermatureDecline(inc,
		turnoverParams);

	return Rcpp::List::create(
		Rcpp::Named("Growth") = ComputeGrowthCoordinateMatrices(pn, inc),
		Rcpp::Named("OvermatureDecline") = ComputeOverMatureDeclineCoordinateMatrices(pn, decline)
		);
}

bool SlowRotationThreshold(double lastSlow, double currentSlow)
{
	if (lastSlow == 0)
	{
		return false;
	}
	else
	{
        return currentSlow / lastSlow > 0.999
            && currentSlow / lastSlow < 1.001;
	}
}

enum SpinupState {
	HistoricRotation = 0, // the historic looping at regular return intervals
	EndOfHistoricRotation = 1, // reached the end of a regular rotation, but not the last rotation, so apply the historic disturbance event
	EndOfFinalRotationThenGrow = 2, //reached the end of the final rotation, so apply the last pass disturbance event and grow the stand to final age
	EndOfFinalRotationThenDelay = 3, //reached the end of the final rotation, and the final stand age is 0, but there is delay
	EndOfFinalRotationThenStop = 4, //reached the end of the final rotation, and the final stand age is 0, but there is delay
	GrowToAge = 5, //after the last pass disturbance event, grow to the stand age
	Delay = 6, //run the delay procedure (decays DOM pools in a deforested stand)
	Disabled = 7 // no more C dynamics
};

SpinupState UpdateSpinupState(SpinupState current, int finalAge, int stepNum,
	int returnInterval, double currentSlow, double lastSlow, int rotationNum,
	int minRotations, int maxRotations, int delay) {
	switch (current){
	case HistoricRotation:
		if (stepNum >= returnInterval) { //check if we are at the criteria for entering the final rotation
			if (rotationNum >= maxRotations - 1 ||
				(SlowRotationThreshold(currentSlow, lastSlow) &&
				rotationNum >= minRotations)) {
			  if (finalAge <= 0) {
			    if (delay <= 0){
			      //shortcut to disabled if there is no delay required
			      return EndOfFinalRotationThenStop;
			    }
			    return EndOfFinalRotationThenDelay;
			  }
			  else {
			    return EndOfFinalRotationThenGrow;
			  }
			}
			else {
				return EndOfHistoricRotation;// historic event occurs
			}
		}
		else{
			return HistoricRotation; // continue the historic rotation
		}
	case EndOfHistoricRotation:
		return HistoricRotation; //loop back to the HistoricRotation state
	case EndOfFinalRotationThenGrow:
    return GrowToAge;
	case EndOfFinalRotationThenDelay:
	  return Delay;
	case EndOfFinalRotationThenStop:
	  return Disabled;
	case GrowToAge:
		if (stepNum == finalAge) {
			if (delay <= 0){
				//shortcut to disabled if there is no delay required
				return Disabled;
			}
			return Delay;
		}
		else {
			return GrowToAge;
		}
	case Delay:
		if (stepNum >= delay - 1) {
			//delay finished
			return Disabled;
		}
		return Delay;
	case Disabled:
		return Disabled;
	}
}

//' Spinup a landscape by running rotations of stand replacing disturbances repeatedly until the
//' pre-disturbance slow pools and the last rotation pre-disturbance slow pools are within a tolerance.
//' Stand then grows to inventory age, and is ready for CBM simulation.
//'
//' @param constantProcesses a list of constant process C dynamics matrices
//' @param growthIncrements a hash table of growth increments by gcid, by age
//' @param ages the stand age, (the inventory age) stands will be simulated to this age in the final pass
//' @param gcids the growth curve ids (referenced by @param growthIncrements)
//' @param spatial_units the cbm-cfs3 spatial unit id
// [[Rcpp::export]]
Rcpp::NumericMatrix Spinup(Rcpp::NumericMatrix& pools,
	Rcpp::IntegerMatrix opMatrix,
	Rcpp::List& constantProcesses,
	Rcpp::Environment& growthIncrements,
	Rcpp::IntegerVector& ages,
	Rcpp::IntegerVector& gcids,
	Rcpp::IntegerVector& historicdmids,
	Rcpp::IntegerVector& lastPassdmids,
	Rcpp::IntegerVector& delays,
	Rcpp::IntegerVector& minRotations,
	Rcpp::IntegerVector& maxRotations,
	Rcpp::IntegerVector& returnIntervals,
	Rcpp::DataFrame& rootParameters,
	Rcpp::DataFrame& turnoverParams,
	double biomassToCarbonRate,
	bool debug = false) {

	R_len_t nstands = ages.length();
	if (nstands <= 0) {
		return Rcpp::NumericMatrix(1, 1);
	}
	Rcpp::NumericMatrix poolsClone(clone(pools));
	PoolNames pn;
	std::vector<SpinupState> state(nstands, HistoricRotation);
	Rcpp::IntegerVector stepNum(nstands, 0);
	std::vector<int> rotationNum(nstands, 0);
	std::vector<double> lastRotationSlow(nstands, 0.0);
	Rcpp::IntegerMatrix disturbanceMatrixIds(nstands, 1);
	std::fill(disturbanceMatrixIds.begin(), disturbanceMatrixIds.end(), 0);
	Rcpp::List disturbanceProcesses = Rcpp::List::create(
		constantProcesses["disturbanceMatrices"]);
	R_len_t finishedCount = 0;
	std::vector<Rcpp::NumericVector> debugRows;

	while (finishedCount != nstands) {

		Rcpp::List growthAndDecline = ComputeGrowthAndDeclineMatrices2(growthIncrements, stepNum,
			gcids, poolsClone, rootParameters, turnoverParams, biomassToCarbonRate, 0.5, 0.5);

		Rcpp::List annualprocesses = Rcpp::List::create(
			growthAndDecline["Growth"],
			constantProcesses["domTurnover"],
			constantProcesses["bioTurnover"],
			growthAndDecline["OvermatureDecline"],
			growthAndDecline["Growth"],
			constantProcesses["domDecayMatrices"],
			constantProcesses["slowDecayMatrices"],
			constantProcesses["slowMixingMatrix"]
			);

		//todo: apply the growth/annual processes step function call here
		StepPoolsRef(poolsClone, opMatrix, annualprocesses);

		for (R_len_t i = 0; i < nstands; i++){
		  stepNum[i] ++;
			
			double totalSlowC = poolsClone[pn.BelowGroundSlowSoil] + poolsClone[pn.AboveGroundSlowSoil];

			SpinupState newState = UpdateSpinupState(state[i], ages[i], stepNum[i],
				returnIntervals[i], totalSlowC, lastRotationSlow[i], rotationNum[i],
				minRotations[i], maxRotations[i], delays[i]);

			if (newState != state[i]){
				state[i] = newState;
				switch (newState){
				case HistoricRotation:
					disturbanceMatrixIds(i, 0) = 0; //no disturbance
					break;
				case EndOfHistoricRotation:
					lastRotationSlow[i] = totalSlowC; //save the slow C from just before the disturbance for the next rotation
					disturbanceMatrixIds(i, 0) = historicdmids[i]; //do historic disturbance
					stepNum[i] = 0;
					rotationNum[i] ++;//increment rotation num
					break;
				case EndOfFinalRotationThenGrow:
					disturbanceMatrixIds(i, 0) = lastPassdmids[i]; //do last pass disturbance
				  stepNum[i] = 0;
					break;
				case EndOfFinalRotationThenDelay:
				  //turn off growth (do delay)
				  opMatrix(i,0) = 0;//growth1
				  opMatrix(i,3) = 0;//overmaturedecline
				  opMatrix(i,4) = 0;//growth2
				  disturbanceMatrixIds(i, 0) = lastPassdmids[i]; //do last pass disturbance
				  stepNum[i] = 0;
				  break;
				case EndOfFinalRotationThenStop:
				  //do last pass disturbance
				  disturbanceMatrixIds(i, 0) = lastPassdmids[i]; 
				  //turn off all dynamics
				  opMatrix(i, Rcpp::_) = Rcpp::IntegerVector(opMatrix.ncol(), 0);
				  finishedCount++;
				  stepNum[i] = 0;
				  break;
				case GrowToAge:
					disturbanceMatrixIds(i, 0) = 0; //no disturbance
					break;
				case Delay:
					opMatrix(i,0) = 0;//growth1
			    opMatrix(i,3) = 0;//overmaturedecline
			    opMatrix(i,4) = 0;//growth2
			    disturbanceMatrixIds(i, 0) = 0; //no disturbance
					break;
				case Disabled:
					//turn off all dynamics
					opMatrix(i, Rcpp::_) = Rcpp::IntegerVector(opMatrix.ncol(), 0);
					finishedCount++;
					break;
				}
			}
		}
		//apply disturbance C dynamics
		// (depending on the value set in the vector "disturbanceMatrixIds")
		//here by calling step function with a single column
		StepPoolsRef(poolsClone, disturbanceMatrixIds, disturbanceProcesses);

		if (debug) {
			for (R_len_t i = 0; i < nstands; i++) {
				Rcpp::NumericVector debugRow(pn.NPools + 2);
				debugRow[0] = i + 1;
				debugRow[1] = stepNum[i];
				for (int pool = 0; pool < pn.NPools; pool++){
					debugRow[2 + pool] = poolsClone(i, pool);
				}
				debugRows.push_back(debugRow);
			}
		}
	}

	if (!debug){
		return poolsClone;
	}
	else{

		Rcpp::NumericMatrix debugMat(debugRows.size(), pn.NPools + 2);
		for (R_len_t r = 0; r < debugMat.nrow(); r++){
			debugMat(r, Rcpp::_) = debugRows[r];
		}

		return debugMat;
	}
}

