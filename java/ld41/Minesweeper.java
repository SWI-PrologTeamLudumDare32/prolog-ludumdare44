/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ld41;
import java.util.Random;

/**
 *
 * @author aaron
 */
public class Minesweeper 
{
    public static final int DEFAULT_ROWS_COLS = 10;
	
	public static final char SPACE_EMPTY = 'x';
	public static final char SPACE_FLAG = 'f';
	public static final char SPACE_UNKNOWN = '#';
	public static final char SPACE_BOMB = 'b';
	public static final char SPACE_INVALID = '*';
	public static final char SPACE_QUESTION = '?';
	
	public static final String CMD_CLICK = "c";
	public static final String CMD_DEBUG = "d";
	public static final String CMD_FLAG = "f";
	public static final String CMD_QUESTION = "?";
	public static final String CMD_REMOVE = "x";
	
	public static final String RESULT_INVALID = "invalid.";
	public static final String RESULT_WIN = "win.";
	public static final String RESULT_LOSE = "lose.";
    
	/// The board with the actual layout
    private char[][] _fullBoard;
	/// The board the user sees
    private char[][] _userBoard;
	
	/// The player's (row, col) locations
	private int playerRow;
	private int playerCol;
	
	/** Default constructor. Just calls the other one with the default rows/cols
	 * 
	 */
	public Minesweeper()
    {
		this(DEFAULT_ROWS_COLS, DEFAULT_ROWS_COLS);
    }
    
	/** Constructor. Sets up both the user and full boards and places the bombs.
	 * 
	 * @param rows The number of rows in the board
	 * @param cols The number of columns of the board
	 */
    public Minesweeper(int rows, int cols)
    {	
		_fullBoard = new char[rows][cols];
		_userBoard = new char[rows][cols];
		
		// Initialize the boards. The user can't see anything and the real board is empty
		for (int i = 0; i < rows; i++)
		{
			for (int j = 0; j < cols; j++)
			{
				_fullBoard[i][j] = SPACE_EMPTY;
				_userBoard[i][j] = SPACE_UNKNOWN;
			}
		}
		
		placeBombs();
		
		calculateNumbers();
		
//		prettyPrintFullGrid(); // For testing only
    }
	
	/** Sets bombs on 10% of the squares. We don't allow a bomb on the center
	 * space (round down where necessary), since the user will always start there.
	 * 
	 */
	private void placeBombs()
	{
		int rows = _fullBoard.length;
		int cols = _fullBoard[0].length;
		
		// Add some bombs! 10% of the squares will contain a bomb
		int numBombs = 0;
		int totalBombs = (int) Math.round(rows * cols * 0.1);
		Random r = new Random(System.currentTimeMillis());
		while (numBombs < totalBombs)
		{
			int row = r.nextInt(rows);
			int col = r.nextInt(cols);

			// If there isn't already a bomb here, put one
			// We also don't allow a bomb on the 'center' space, since that's where the user starts
			if (_fullBoard[row][col] == SPACE_EMPTY && row != Math.round(rows / 2) && col != Math.round(cols / 2))
			{
				_fullBoard[row][col] = SPACE_BOMB;
				numBombs++;
			}
			// Otherwise, don't do anything - just try again next iteration
		}
		
		// Set the default player location
		playerRow = Math.round(rows / 2);
		playerCol = Math.round(cols / 2);
	}
	
	/** Calculates the numbers for the board
	 * 
	 */
	private void calculateNumbers()
	{	
		for (int x = 0; x < _fullBoard.length; x++)
		{
			for (int y = 0; y < _fullBoard[0].length; y++)
			{
				int adjacentBombs = 0;
				// Check the squares around the current square for bombs
				for (int i = (x - 1); i < (x + 2); i++)
				{
					for (int j = (y - 1); j < (y + 2); j++)
					{
						if (i >= 0 && i < _fullBoard.length &&
							j >= 0 && j < _fullBoard[0].length)
						{
							if (i != x || j != y)
							{
								if (_fullBoard[i][j] == SPACE_BOMB)
								{
									adjacentBombs++;
								}
							}
						}
					}
				}
				// Note: if there's no bombs around a space, it shows the SPACE_EMPTY character, not 0
				if (_fullBoard[x][y] != SPACE_BOMB && adjacentBombs > 0)
				{
					_fullBoard[x][y] = Integer.toString(adjacentBombs).charAt(0);
				}
			}
		}
	}

	/** Process a command from the prolog program. Format is 'X Y,Z', where X
	 * is a command character (see CMD_* values) and Y and Z are the row and
	 * column, respectively.
	 * 
	 * @param command The action to perform on the board
	 * @param values The square on the board
	 * @return A string to be sent back to the prolog program
	 */
    public String processCommand(String command, String values)
	{
		if (command.toLowerCase().equals(CMD_DEBUG))
		{
			return "debug(\'" + getUserGrid() + "\', \'" + getFullGrid() + "\').\n";
		}
		else if (command.toLowerCase().equals("u"))	// For testing only
		{
			prettyPrintUserGrid();
			return "";
		}
		else if (command.toLowerCase().equals("a")) // For testing only
		{
			prettyPrintFullGrid();
			return "";
		}
		else if (!values.contains(",") || values.length() < 3)
		{
			return RESULT_INVALID;
		}
		else
		{
			String[] coordinates = values.split(",");
			int x = Integer.parseInt(coordinates[0]);
			int y = Integer.parseInt(coordinates[1]);

			String result = RESULT_INVALID;
			if (x >= 0 && x < _fullBoard.length && y >= 0 && y < _fullBoard[0].length)
			{
				if (command.toLowerCase().equals(CMD_CLICK))
				{
					result = clickSquare(x, y);
					
					if (!result.equals(RESULT_LOSE) && !result.equals(RESULT_WIN))
					{
						// If the game isn't over, this command actually moves 
						// the player, so update their location
						playerRow = x;
						playerCol = y;
					}
				}
				else 
				{
					if (command.toLowerCase().equals(CMD_FLAG))
					{
						flagSquare(x, y);
					}
					else if (command.equals(CMD_QUESTION))
					{
						questionSquare(x, y);
					}
					else if (command.toLowerCase().equals(CMD_REMOVE))
					{
						clearSquare(x, y);
					}
					
					if (allBombsFlagged())
					{
						result = RESULT_WIN;
					}
					else
					{
						// These commands don't move the player, so use the last
						// saved location
						result = peek(playerRow, playerCol);
					}
				}
			}

			return result;
		}
	}
	
	/** Processes a click command for a square
	 * 
	 * @param x The row in the board
	 * @param y The column in the board
	 * @return A string showing the 3 x 3 grid around the location or a win/lose result
	 */
	private String clickSquare(int x, int y)
	{	
		_userBoard[x][y] = _fullBoard[x][y];
		
		if (_userBoard[x][y] == SPACE_BOMB)
		{
			return RESULT_LOSE;
		}
		if (allBombsFlagged())
		{
			return RESULT_WIN;
		}
		
		// Update the user board to uncover any blank spaces
		if (_userBoard[x][y] == SPACE_EMPTY)
		{
			uncoverBlanks(x, y);
		}
		return peek(x, y);
	}
	
	/** Processes a flag command for a square. Unknown or '?' spaces can be marked
	 * with a flag.
	 * 
	 * @param x The row in the board
	 * @param y The column in the board
	 */
	private void flagSquare(int x, int y)
	{
		if (_userBoard[x][y] == SPACE_UNKNOWN || _userBoard[x][y] == SPACE_QUESTION)
		{
			_userBoard[x][y] = SPACE_FLAG;
		}
	}
	
	/** Processes a question command on a square. Unknown or flagged spaces can
	 * be marked with a question mark.
	 * 
	 * @param x The row in the board
	 * @param y The column in the board
	 */
	private void questionSquare(int x, int y)
	{
		if (_userBoard[x][y] == SPACE_UNKNOWN || _userBoard[x][y] == SPACE_FLAG)
		{
			_userBoard[x][y] = SPACE_QUESTION;
		}
	}
	
	/** Processes a remove command on a square. Only flags and question marks
	 * can be removed.
	 * 
	 * @param x The row in the board
	 * @param y The column in the board
	 */
	private void clearSquare(int x, int y)
	{
		if (_userBoard[x][y] == SPACE_FLAG || _userBoard[x][y] == SPACE_QUESTION)
		{
			_userBoard[x][y] = SPACE_UNKNOWN;
		}
	}
	
	/** Determines if all bombs in the full board have been flagged and no extra
	 * flags are present.
	 * 
	 * @return true if all bombs are flagged, and false otherwise
	 */
	private boolean allBombsFlagged()
	{
		for (int i = 0; i < _fullBoard.length; i++)
		{
			for (int j = 0; j < _fullBoard[0].length; j++)
			{
				// If a bomb is not flagged or a flag is NOT on a bomb, then just bail out
				if ((_fullBoard[i][j] == SPACE_BOMB && _userBoard[i][j] != SPACE_FLAG) ||
					(_fullBoard[i][j] != SPACE_BOMB && _userBoard[i][j] == SPACE_FLAG))
				{
					return false;
				}
			}
		}
		
		// We didn't find an unflagged bomb
		return true;
	}
	
	/** Called after the user clicked a square to uncover any connected blanks
	 * 
	 */
	private void uncoverBlanks(int x, int y)
	{
		boolean adjacentBombs = false;
		int[][] locationsToCheck = new int[8][2];
		int next = 0;
		
		// Count bombs adjacent to a square. 
		for (int i = (x - 1); i < (x + 2); i++)
		{
			for (int j = (y - 1); j < (y + 2); j++)
			{
				if (i >= 0 && i < _userBoard.length &&
					j >= 0 && j < _userBoard[0].length)
				{
					if (i != x || j != y)
					{
						if (_fullBoard[i][j] == SPACE_BOMB)
						{
							adjacentBombs = true;
							break;
						}
						else
						{
							locationsToCheck[next][0] = i;
							locationsToCheck[next][1] = j;
							next++;
						}
					}
				}
			}
		}
		
		// If no adjacent bombs, uncover all adjacent squares and make a recursive
		// call to this function for each square that's not already uncovered
		if (!adjacentBombs)
		{
			boolean[] processLocation = new boolean[8];
			for (int i = 0; i < next; i++)
			{
				int xCoord = locationsToCheck[i][0];
				int yCoord = locationsToCheck[i][1];
				
				processLocation[i] = false;
				if (_userBoard[xCoord][yCoord] == SPACE_UNKNOWN)
				{
					_userBoard[xCoord][yCoord] = _fullBoard[xCoord][yCoord];
					processLocation[i] = true;
				}
			}
			
			for (int k = 0; k < next; k++)
			{
				if (processLocation[k])
				{
					int xCoord = locationsToCheck[k][0];
					int yCoord = locationsToCheck[k][1];

					uncoverBlanks(xCoord, yCoord);
				}
			}
		}
	}

	/** Gets a copy of the user's board in a format suitable for the prolog program
	 * 
	 * @return The string
	 */
	private String getUserGrid()
	{
		String s = "";
		
		for (char[] row : _userBoard)
		{
			for (char square : row)
			{
				s += square;
			}
		}
		return s;
	}
	
	/** Gets a copy of the full board in a format suitable for the prolog program
	 * 
	 * @return 
	 */
	private String getFullGrid()
	{
		String s = "";

		for (char[] row : _fullBoard)
		{
			for (char square : row)
			{
				s += square;
			}
		}

		return s;
	}
	
	/** Returns a string representing the 3x3 grid around a location on the user's board
	 * 
	 * @param x The row on the board
	 * @param y The column on the board
	 * @return The string in a prolog-friendly format
	 */
	private String peek(int x, int y)
	{
		String s = "click([";
		
		for (int i = (x - 1); i < (x + 2); i++)
		{
			s += "[";
			for (int j = (y - 1); j < (y + 2); j++)
			{
				if (i < 0 || j < 0 || i >= _userBoard.length || j >= _userBoard[0].length)
				{
					s += SPACE_INVALID;
				}
				else
				{
					s += _userBoard[i][j];
				}
				
				if (j != y + 1)
				{
					s += ", ";
				}
			}
			s += "]";
			if (i != x + 1)
			{
				s += ", ";
			}
		}
		
		s += "]).";
		return s;
	}
	
	/** Debugging function that neatly prints the user board
	 * 
	 */
	private void prettyPrintUserGrid()
	{
		String s = "";
		
		for (char[] row : _userBoard)
		{
			for (char square : row)
			{
				s += square + " ";
			}
			s += "\n";
		}
		
		System.out.print(s);
	}
	
	/** Debugging function that neatly prints the full board
	 * 
	 */
	private void prettyPrintFullGrid()
	{
		String s = "";
		for (char[] row : _fullBoard)
		{
			for (char square : row)
			{
				s += square + " ";
			}
			s += "\n";
		}
		
		System.out.print(s);
	}
}
