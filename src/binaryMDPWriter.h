#ifndef HMDPWRITER_HPP
#define HMDPWRITER_HPP

//-----------------------------------------------------------------------------
//#include <stdlib.h>     // For use of exit command
//#include <stdio.h>      // For use of scanf and printf
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include "basicdt.h"
#include "debug.h"
//#include "time.h"
using namespace std;

typedef double flt;                 ///< A floating number datatype.

///** Global function for converting a number to a string */
//template <typename T>
//std::string inline ToString(T t) {
//    std::ostringstream s;
//    s << t;
//    return s.str();
//};
// -----------------------------------------------------------------------------


/** Class for writing a HMDP model to binary files.

See the documentation of the binaryMDPWriter in R.
 */
class BinaryMDPWriter
{

private:

    /** Write value to binary file. */
    void WriteBinary(FILE* pFile, const vector<int> &vec) {
        fwrite(&vec[0], sizeof(int), vec.size(), pFile);
        //cout << "W (v(int)): "; for(idx ii=0; ii < vec.size(); ii++) cout << vec[ii] << " " << flush; cout << endl;
    }

    /** Write value to binary file. */
    void WriteBinary(FILE* pFile, const vector<flt> &vec) {
        fwrite(&vec[0], sizeof(flt), vec.size(), pFile);
        //cout << "W (v(flt)): "; for(idx ii=0; ii < vec.size(); ii++) cout << vec[ii] << " " << flush; cout << endl;
    }

    /** Write value to binary file. */
    void WriteBinary(FILE* pFile, const int i) {
        fwrite(&i, sizeof(int), 1, pFile);
        //cout << "W (int): " << i << flush; cout << endl;
    }

    /** Write value to binary file. */
    void WriteBinary(FILE* pFile, const flt i) {
        fwrite(&i, sizeof(flt), 1, pFile);
        //cout << "W (flt): " << i << flush; cout << endl;
    }

    /** Write value to binary file. */
    void WriteBinary(FILE* pFile, const string &str) {
        fwrite(str.c_str(), sizeof(char), str.length()+1, pFile);   // add the null character also
        //cout << "W (string): " << str << flush; cout << endl;
    }

public:

    /** Constructor. */
    BinaryMDPWriter(){BinaryMDPWriter("");};

    /** Open binary files for writing. */
    BinaryMDPWriter(string prefix) {
        string stateIdxFileN = prefix + "stateIdx.bin";
        string stateIdxLblFileN = prefix + "stateIdxLbl.bin";
        string actionIdxFileN = prefix + "actionIdx.bin";
        string actionIdxLblFileN = prefix + "actionIdxLbl.bin";
        string actionWFileN = prefix + "actionWeight.bin";
        string actionWLblFileN = prefix + "actionWeightLbl.bin";
        string transProbFileN = prefix + "transProb.bin";
        string externalProcessesFileN = prefix + "externalProcesses.bin";
        closed = false;
        wFixed=false;
        wLblLth=sTotal=aTotal=0;
        aCtr=-1;
        //timer.StartTimer();
        pStateIdxFile = fopen(stateIdxFileN.c_str(), "wb");
        pStateIdxLblFile = fopen(stateIdxLblFileN.c_str(), "wb");
        pActionIdxFile = fopen(actionIdxFileN.c_str(), "wb");
        pActionIdxLblFile = fopen(actionIdxLblFileN.c_str(), "wb");
        pActionWFile = fopen(actionWFileN.c_str(), "wb");
        pActionWLblFile = fopen(actionWLblFileN.c_str(), "wb");
        pTransProbFile = fopen(transProbFileN.c_str(), "wb");
        pExternalProcessesFile = fopen(externalProcessesFileN.c_str(), "wb");
    }

    /** Close binary files. */
    void CloseWriter() {
      fclose(pStateIdxFile);
      fclose(pStateIdxLblFile);
      fclose(pActionIdxFile);
      fclose(pActionIdxLblFile);
      fclose(pActionWFile);
      fclose(pActionWLblFile);
      fclose(pTransProbFile);
      fclose(pExternalProcessesFile);
      //timer.StopTimer();
      closed = true;

      log << "Create HMDP ...\n\n";
       log << "  Statistics:\n";
      log << "    states : " << sTotal << "\n";
      log << "    actions: " << aTotal << "\n";
      log << "    weights: " << wLblLth << "\n\n";
      log << "  Closing binary MDP writer.\n\n";
      //log << "  Total time for writing to binary files: " << timer.ElapsedTime("sec") << " sec.\n\n";
    }

    ~BinaryMDPWriter() {
       if (!closed) CloseWriter();
    }

    /** Add a weight name to the binary files.
     * \param label The label of the weight.
     */
    void SetWeight(const string &label) {
        if (wFixed) {
            log << "Error: can not add weight label!\n";
            return;
        }
        //WriteBinary<char*>(actionWLblFile, label.c_str())
        WriteBinary(pActionWLblFile, label);
        wLblLth++;
    }

    /** Add weight names to the binary files.
     * \param label The label of the weight.
     */
    void SetWeights(const vector<string> &labels) {
        if (wFixed) {
            log << "Error: can not add weight label!\n";
            return;
        }
        for (size_t i=0;i<labels.size();i++) WriteBinary(pActionWLblFile, labels[i]);
        wLblLth = wLblLth + labels.size();
    }

    /** Start a process. */
    void Process() {
        wFixed=true;
        iHMDP.push_back(-1);   // add stage idx
        iHMDP.push_back(-1);   // add state idx
        sId.push_back(-1);     // add state id for process (-1 = unknown)
        aCtr=-1;  // start with a new state
        //cout << "Start proc " << GetIHMDP() << endl;
    }

    /** End a process. */
    void EndProcess() {
        iHMDP.pop_back();   // remove state
        iHMDP.pop_back();   // remove stage
        aCtr=iHMDP.back();  // get action index
        sId.pop_back();     // remove state id at this level
        //cout << "End proc " << GetIHMDP() << endl;
    }

    /** Start a stage. */
    void Stage() {
        iHMDP[iHMDP.size()-2]++;    // increment stage index
        //cout << "Start stage " << GetIHMDP() << endl;
    }

    /** End a stage. */
    void EndStage() {
        iHMDP[iHMDP.size()-1]=-1;    // reset state index
        //cout << "End stage " << GetIHMDP() << endl;
    }

    /** Add a state.
     * \param label The label of the state.
     */
    int State(const string &label) {
        sTotal++;
        sId.back() = sTotal-1;  // store state id (if create child process)
        iHMDP[iHMDP.size()-1]++; // increment state index
        AddState(iHMDP, label);
        //cout << "Start state " << label << " " << GetIHMDP() << endl;
        return sTotal-1;
    }

    /** End state. */
    void EndState() {
        aCtr=-1;    // reset action ctr
        //cout << "End state " << GetIHMDP() << endl;
    }

    /** Add an action.
     * \param scope Scope of the transition probabilities.
     * \param index Index of the transition probabilities.
     * \param prob Transition probabilities.
     * \param weights Weights of the action.
     * \param label The label of the action.
     * \param end True if not an action that define a child process.
     */
    void Action(const vector<int> &scope, const vector<int> &index,
                   const vector<flt> &prob, const vector<flt> &weights, const string &label, bool end) {
        aTotal++;
        aCtr++;
        iHMDP.push_back(aCtr);
        AddAction(sId.back(), scope, index, prob, weights, label);
        //cout << "Start action " << label << " " << GetIHMDP() << endl;
        if (end) EndAction();
    }

    /** End action. */
    void EndAction() {
        iHMDP.pop_back();
        //cout << "End action " << GetIHMDP() << endl;
    }

   /** Add an external process.
     * \param prefix The prefix of the external process
     * \param scope Scope of the transition probabilities.
     * \param index Index of the transition probabilities.
     * \param prob Transition probabilities.
     * \param weights Weights of the action.
     * \param label The label of the action.
     * \param termStates Number of states at the last stage in the external process.
     */
    vector<int> IncludeProcess(const string &prefix, const vector<int> &scope, const vector<int> &index,
                   const vector<flt> &prob, const vector<flt> &weights, const string &label, const int &termStates){
        vector<int> ids;
        aTotal++;
        aCtr++;
        iHMDP.push_back(aCtr);
        AddAction(sId.back(), scope, index, prob, weights, label);
        // create first stage
        int maxIdx = *max_element(index.begin(),index.end());
        vector<int> scp(termStates,1);
        vector<int> idx;
        for (int i=0; i<termStates; ++i) idx.push_back(i);
        vector<flt> pr(termStates,1/(flt)termStates);
        vector<flt> w(wLblLth,0);
        string lbl;
        Process();
          Stage();
            WriteBinary(pExternalProcessesFile, StageStr());
            WriteBinary(pExternalProcessesFile, prefix);
            //WriteBinary(pExternalProcessesFile, ToString<int>(-1));
            for (int i=0; i<=maxIdx; ++i) {
                ids.push_back( State(lbl) );
                  Action(scp, idx, pr, w, lbl, true);
                EndState();
            }
          EndStage();
          return ids;
    }

    /** End include process. */
    void EndIncludeProcess() {
       EndProcess();
       iHMDP.pop_back();
    }

    /** Add a state to the files stateIdx.bin and stateIdxLbl.bin.
     * \param index The index vector of the HMDP state. Always of size
     * 2+3*level, e.g vector [0,1,0,3,2] says that we consider stage 0,
     * state 1 and action 0 at the founder and stage 3 and state 2 at level one.
     * \param label The label of the state.
     */
    void AddState(const vector<int> &index, const string &label) {
        WriteBinary(pStateIdxFile, index);
        WriteBinary(pStateIdxFile, (int)-1);
        if (label.length()>0) {
            WriteBinary(pStateIdxLblFile, ToString<int>(sTotal-1));
            WriteBinary(pStateIdxLblFile, label);
        }
    }

    /** Add a state to the files stateIdx.bin.
     * \param index The index vector of the HMDP state. Always of size
     * 2+3*level, e.g vector [0,1,0,3,2] says that we consider stage 0,
     * state 1 and action 0 at the founder and stage 3 and state 2 at level one.
     */
    void AddState(const vector<int> &index) {
        AddState(index, "");
    }

    /** Add a action to the binary files.
     * \param sId Id of the state.
     * \param scope Scope of the transition probabilities.
     * \param index Index of the transition probabilities.
     * \param prob Transition probabilities.
     * \param weights Weights of the action.
     * \param label The label of the action.
     */
    void AddAction(int sId, const vector<int> &scope, const vector<int> &index,
                   const vector<flt> &prob, const vector<flt> &weights, const string &label) {
        WriteBinary(pActionIdxFile, sId);
        for (size_t i=0; i<scope.size();i++) {
            WriteBinary(pActionIdxFile, scope[i]);
            WriteBinary(pActionIdxFile, index[i]);
        }
        WriteBinary(pActionIdxFile, (int)-1);
        if (label.length()>0) {
            WriteBinary(pActionIdxLblFile, ToString<int>(aTotal-1));
            WriteBinary(pActionIdxLblFile, label);
        }
        WriteBinary(pActionWFile, weights);
        WriteBinary(pTransProbFile, prob);
        WriteBinary(pTransProbFile, (flt)-1);
    }

    // Output the index vector
    //string GetIHMDP() {return vec2String<int>(iHMDP);}

    /** Stage string. */
    string StageStr() {
        std::ostringstream s;
        for (size_t i=0; i<iHMDP.size()-2; ++i) s << iHMDP[i] << ",";
        s << iHMDP[iHMDP.size()-2];
        return s.str();
    }

public:
    bool closed;            ///< True if writer closed.
    ostringstream log;
private:
    FILE* pStateIdxFile;
    FILE* pStateIdxLblFile;
    FILE* pActionIdxFile;
    FILE* pActionIdxLblFile;
    FILE* pActionWFile;
    FILE* pActionWLblFile;
    FILE* pTransProbFile;
    FILE* pExternalProcessesFile;

	vector<int> iHMDP; ///< Index of the HMDP state (use int since store int in binary file).
   vector<int> sId;   ///< Containing the state id's (used to find the state id the action is defined under)
	int sTotal; ///< Total number of states.
	int aTotal; ///< Total number of actions.
	int aCtr; ///< current action at current state
	int wLblLth;
	bool wFixed; ///< TRUE if size of weights are fixed
   //Timer timer; ///< Cpu measurement
};

// -----------------------------------------------------------------------------

#endif
