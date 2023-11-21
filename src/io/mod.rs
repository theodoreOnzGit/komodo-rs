use core::time;
use std::{thread, io::{self, Write, Read}, path::PathBuf, fs::File};

pub mod cli_parse;

use clap::Parser;

use crate::sdata::CalculationMode;

use self::cli_parse::KomodoArgs;

/// reads input somehow
pub fn inp_read() -> InputReadStruct {
    // from fortran code:
    // IMPLICIT NONE
    // SAVE
    // 
    // IMPLICIT NONE disables 
    // implicit typing, not needed to translate
    //
    // SAVE essentially lets the variable get saved 
    // across multiple files. Sort of like global variables 
    // I'm not doing this as I strongly favour functional 
    // or object oriented programming
    //
    // in general, static variables are discouraged
    // construct various structs and return 

    let io_options = InputOutputOptions::default();

    // fortran:
    // CHARACTER(LEN=100) :: iname, oname
    //
    // ! ind is sed to read x indicator in beginning of input buffer line.
    // ! This to prevent reading next line
    // CHARACTER(LEN=1) :: ind

    let iname: String;
    let mut oname: String;
    let ind: char;

    let mode: CalculationMode;

    // start reading lots of input
    //
    // fortran:
    // ! Input, output and buffer input file unit number
    // INTEGER, PARAMETER :: iunit = 100   !input file unit number
    // INTEGER, PARAMETER :: ounit = 101   !output file unit number
    // INTEGER, PARAMETER :: buff  = 99    !input buffer file unit number (entire input)

    let iunit: i32 = 100;
    let ounit: i32 = 100;
    let buff: i32 = 99;

    // fortran: 
    //
    // ! Input buffer file unit number for each card
    // INTEGER, PARAMETER :: umode = 111, uxsec = 112, ugeom = 113, ucase = 114
    // INTEGER, PARAMETER :: uesrc = 115, uiter = 116, uprnt = 117, uadf  = 118
    // INTEGER, PARAMETER :: ucrod = 119, ubcon = 120, uftem = 121, umtem = 122
    // INTEGER, PARAMETER :: ucden = 123, ucbcs = 124, uejct = 125, uther = 126
    // INTEGER, PARAMETER :: uxtab = 127, ukern = 128, uextr = 129, uthet = 130
    // INTEGER, PARAMETER :: uoutp = 131
    // INTEGER :: bunit

    let umode: i32 = 111;  
    let uxsec: i32 = 112;  
    let ugeom: i32 = 113;  
    let ucase: i32 = 114;
    let uesrc: i32 = 115;  
    let uiter: i32 = 116;  
    let uprnt: i32 = 117;  
    let uadf : i32 = 118;
    let ucrod: i32 = 119;  
    let ubcon: i32 = 120;  
    let uftem: i32 = 121;  
    let umtem: i32 = 122;
    let ucden: i32 = 123;  
    let ucbcs: i32 = 124;  
    let uejct: i32 = 125;  
    let uther: i32 = 126;
    let uxtab: i32 = 127;  
    let ukern: i32 = 128;  
    let uextr: i32 = 129;  
    let uthet: i32 = 130;
    let uoutp: i32 = 131;
    let bunit: i32;

    // sort of translation
    // INTEGER :: bmode = 0, bxsec = 0, bgeom = 0, bcase = 0, besrc = 0
    // INTEGER :: biter = 0, bprnt = 0, badf  = 0, bcrod = 0, bbcon = 0
    // INTEGER :: bftem = 0, bmtem = 0, bcden = 0, bcbcs = 0, bejct = 0
    // INTEGER :: bther = 0, bxtab = 0, bkern = 0, bextr = 0, bthet = 0
    // INTEGER :: boutp = 0
    //
    // I changed it into an active/inactive enum

    let bmode = CardActive::Inactive; 
    let bxsec = CardActive::Inactive; 
    let bgeom = CardActive::Inactive; 
    let bcase = CardActive::Inactive; 
    let besrc = CardActive::Inactive;
    let biter = CardActive::Inactive; 
    let bprnt = CardActive::Inactive; 
    let badf  = CardActive::Inactive; 
    let bcrod = CardActive::Inactive; 
    let bbcon = CardActive::Inactive;
    let bftem = CardActive::Inactive; 
    let bmtem = CardActive::Inactive; 
    let bcden = CardActive::Inactive; 
    let bcbcs = CardActive::Inactive; 
    let bejct = CardActive::Inactive;
    let bther = CardActive::Inactive; 
    let bxtab = CardActive::Inactive; 
    let bkern = CardActive::Inactive; 
    let bextr = CardActive::Inactive; 
    let bthet = CardActive::Inactive;
    let boutp = CardActive::Inactive;

    // direct translation:
    // INTEGER :: g, i, N

    let i: i32;
    let g: i32;
    let command_argument_count: i32;


    // direct translation: 
    //
    // !Got this trick from: http://web.utah.edu/thorne/computing/Handy_Fortran_Tricks.pdf
    // N = command_argument_count()
    // IF (N < 1) THEN
    //    WRITE(*,*) '  NOTE : You can also write the input directly after the command'
    //    WRITE(*,'(A,A100)',ADVANCE='NO') '  INPUT NAME : '
    //    READ(*,*) iname
    // ELSE
    //    CALL get_command_argument(1,iname) !Grab the first command line argument
    // ENDIF
    // how does one do input and output in Rust?
    
    // note that the command_argument_count is a fortran function,
    // basically, if no inputs are given, then it will prompt the user 
    // to give an input name

    let args = KomodoArgs::parse();

    // the purpose of the code is to check if the user supplied an 
    // input file. If so, then assign the input to a string
    fn read_input(input: &mut String){
        match io::stdin().read_line(input) {
            Ok(_bytes) => {
                input
            },
            Err(_) => {
                todo!()
            },
        };
    }

    let input_path: PathBuf = match args.input_file {
        Some(filepath) => {
            filepath
        },
        None => {
            // allow the user to input filename manually
            println!("  NOTE : You can also write the input directly after the command");
            println!("  INPUT NAME : ");
            let mut input = String::new();
            read_input(&mut input);

            // probably need to convert this input to a filepath though
            // idk if this works...
            
            let filepath: PathBuf = PathBuf::from(input);

            //dbg!(&filepath);
            //println!("{:?}",filepath);
            filepath
        },
    };
    // once input file supplied open it up
    //
    // fortran code:
    // iname = TRIM(iname)
    // 
    // CALL openFIle (iunit, iname, 'input', 'Input File Open Failed--status')
    // 
    // oname = TRIM(iname) // '.out'
    // oname = TRIM(oname)

    iname = input_path.to_str().unwrap().trim().to_owned();

    dbg!(&iname);
    // now attempt to open the file, otherwise throw an error
    // now I was wondering about fortran's file opening,
    // it requires a "unit" identifier for which every file needs a unit 
    // for Rust, this is not really required, so I'll 
    // probably deprecate this in future rewrites
    // 
    openfile(iunit, iname.clone(), 
        "input".to_string(), "Input File Open Failed--status".to_string());

    // now for the output name,
    oname = iname.clone() + ".out";
    oname = oname.trim().to_string();

    // start writing an output file 
    // 
    // fortran code:
    //
    // OPEN (UNIT=ounit, FILE=oname, STATUS='REPLACE', ACTION='WRITE')
    let mut output_file = File::create(oname).unwrap();


    // scratch files ...
    //
    // I have read that these are temporary files 
    // fortran code:
    // OPEN (UNIT=umode, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uxsec, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ugeom, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ucase, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uesrc, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uiter, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uprnt, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uadf,  STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ucrod, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ubcon, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uftem, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=umtem, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ucden, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ucbcs, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uejct, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uther, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uxtab, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=ukern, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uextr, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uthet, STATUS='SCRATCH', ACTION='READWRITE')
    // OPEN (UNIT=uoutp, STATUS='SCRATCH', ACTION='READWRITE')
    //
    // scratch files are not saved to disk, but essentially 
    // act as temporary files with which one can manipulate

    // allocate basically allocates memory 
    //
    // fortran: 
    //
    // ALLOCATE (farr(ncard)); farr = ADJUSTL(iname)

    // the next step is the input echo, means echo input to output file
    // 
    // fortran: 
    //
    // CALL inp_echo()
    inp_echo(iname, &mut output_file);


    let io_struct = InputReadStruct {
        io_options
    };

    return io_struct;

}

/// contains all the information necessary for the input read
#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct InputReadStruct {
    pub io_options: InputOutputOptions
}

// Translation of fortran code:
// !Ouput options
// LOGICAL, PARAMETER :: ogeom = .TRUE.  ! Geometry output print option
// LOGICAL, PARAMETER :: oxsec = .TRUE.  ! Macroscopic CXs output print option
// LOGICAL, PARAMETER :: scr = .TRUE.    ! Terminal ouput print option
#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct InputOutputOptions {
    pub ogeom: bool,
    pub oxsec: bool,
    pub scr: bool,
}

impl Default for InputOutputOptions {
    fn default() -> Self {
        Self { ogeom: true, oxsec: true, scr: true }
    }
}


#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum CardActive {
    Active,
    Inactive
}

impl Default for CardActive {
    fn default() -> Self {
        CardActive::Inactive
    }
}

impl Into<bool> for CardActive {
    fn into(self) -> bool {
        match self {
            CardActive::Active => true,
            CardActive::Inactive => false,
        }
    }
}

/// translation of: 
///
/// SUBROUTINE openFile(file_unit, iname, file, er_message)
/// 
/// INTEGER :: file_unit
/// CHARACTER(LEN=*) :: iname, file, er_message
/// INTEGER  :: iost
/// 
/// OPEN (UNIT=file_unit, FILE=iname, STATUS='OLD', ACTION='READ', &
///       IOSTAT = iost)
/// 
/// IF (iost /= 0) THEN
///   WRITE(*,1020) er_message, iost
///   WRITE(*,*) '  CANNOT FIND '// file //' FILE : ', iname
///   1020 FORMAT    (2X, A, I6)
///   STOP
/// END IF
/// 
/// END SUBROUTINE openFile
///
/// to be frank, this function merely tries to open the 
/// file, returning an error if you can't open it
///
/// for this, I just try to do a file open 
/// and unwrap basically does the same thing
/// the only reason why the file and error message exists 
/// is to generate the error message
pub fn openfile(_file_unit: i32, iname: String,
    _file: String,
    _er_message: String){

     let file = File::open(&iname);

     file.unwrap();

}

/// translation of:
/// SUBROUTINE inp_echo()
/// !
/// ! Purpose:
/// !    To rewrite the input
/// !
/// 
/// IMPLICIT NONE
/// 
/// INTEGER :: eof
/// INTEGER :: nline
/// 
/// WRITE(ounit, 2409)
/// WRITE(ounit, 2411)
/// WRITE(ounit, 2412)
/// WRITE(ounit, 2409)
/// WRITE(ounit, *)
/// WRITE(ounit, *)
/// 
/// if (scr) then
///   WRITE(*, *)
///   WRITE(*, *)
///   WRITE(*, 2409)
///   WRITE(*, 2411)
///   WRITE(*, 2412)
///   WRITE(*, 2409)
/// #ifdef __GIT
///   WRITE(*, *)
///   WRITE(*, *) "GIT COMMIT SHA    : ", __GIT_COMMIT_HASH
///   WRITE(*, *) "GIT COMMIT DATE   : ", __GIT_DATE
///   WRITE(*, *) "GIT COMMIT BRANCH : ", __GIT_BRANCH
///   WRITE(*, *)
/// #endif
///   WRITE(*, *)
///   WRITE(*, *)
/// end if
/// 
/// 
/// WRITE(ounit,1002) 'STARTS'
/// 
/// nline = 0
/// DO
///    READ(iunit, '(A200)', IOSTAT=eof) iline
///    nline = nline + 1
///    IF (eof < 0) THEN
///        WRITE(ounit,1002) 'ENDS'
///        WRITE(ounit,*)
///        EXIT
///    END IF
///    WRITE(ounit, 1001) nline, iline
/// END DO
/// 
/// 2409 FORMAT(11X, '###########################################################')
/// 2411 FORMAT(11X, '#                 KOMODO Version: 0.2                     #')
/// 2412 FORMAT(11X, '#           OPEN NUCLEAR REACTOR SIMULATOR                #')
/// 
/// 1001 FORMAT (2X, I4, ': ', A200)
/// 1002 FORMAT    (2X, '=============================INPUT DATA',A7, &
///                     ' HERE===========================')
/// REWIND (iunit)
/// 
/// END SUBROUTINE inp_echo
pub fn inp_echo(iname: String,
    output_file: &mut File){

    let mut file = File::open(&iname).unwrap();
    let mut input_string: String =  String::new();


    input_string += "###########################################################\n";
    input_string += "#                 KOMODO-rs Version: 0.1                  #\n";
    input_string += "#           OPEN NUCLEAR REACTOR SIMULATOR                #\n";
    input_string += "###########################################################\n";
    input_string += "\n";
    input_string += "\n";
    input_string += "====================INPUT DATA HERE========================\n";

    file.read_to_string(&mut input_string).unwrap();
    output_file.write_all(&input_string.into_bytes()).unwrap();

}
