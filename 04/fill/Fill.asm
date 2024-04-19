// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.
(MAIN)
// initialize row counter
    @255
    D = A
    @row_index
    M = D
// initialize row address
    @8160
    D = A
    @row_address
    M = D
(ROW_LOOP)
    
// initialize column counter
    @31
    D = A
    @column_index
    M = D
(COLUMN_LOOP)
// prepare to compute the pixel groups address. first load the row index into R0
    @row_address
    D = M
    @R0
    M = D
// load the column index into R1
    @column_index
    D = M
    @R1
    M = D
// call PIXEL_ADDRESS
    @PIXEL_ADDRESS
    0; JMP
(PIXEL_ADDRESS_RETURN)

// do we paint black or white?
    @KBD
    D = M
    @BLACK
    D; JNE
// paint white
// address of the pixel group still lives in R2 so we load it and then write to it
    @R2
    A = M
    M = 0
    @END_IF
    0; JMP
(BLACK)
// red door 
    @R2
    A = M
    M = -1
(END_IF)
    
// decrement the column counter and jump back if necessary
    @column_index
    M = M - 1
    D = M
    @COLUMN_LOOP
    D; JGE

// decrement address of the current row
    @32
    D = A
    @row_address
    M = M - D
    
// decrement the row counter and jump back if necessary
    @row_index
    M = M - 1
    D = M
    @ROW_LOOP
    D; JGE

// jump back to main
    @MAIN
    0; JMP

// takes the current rows adress in R0 and the current column in R1, calculates
// the address of that column and returns it in R2 
(PIXEL_ADDRESS)
// add the screens base address and the column index to the row address
    @SCREEN
    D = A
    @R0
    D = D + M
    @R1
    D = D + M
    @R2
    M = D
    @PIXEL_ADDRESS_RETURN
    0; JMP
