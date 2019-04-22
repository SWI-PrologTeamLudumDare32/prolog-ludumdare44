/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ld41;

import java.util.Scanner;
import java.util.Timer;
import java.util.TimerTask;

/**
 *
 * @author aaron
 */
public class Ld41 
{
    /** TODO: Setup watchdog to kill if no input for 5 minutes
     * @param args the command line arguments
     */
    public static void main(String[] args)
    {
		int rows = Minesweeper.DEFAULT_ROWS_COLS;
		int cols = Minesweeper.DEFAULT_ROWS_COLS;

		// Try to read the rows and height.
		if (args.length >= 1)
		{
			try
			{
				rows = Integer.parseInt(args[0]);
			}
			catch (Exception e)
			{
				// Leave rows at the default
			}
		}
		if (args.length >= 2)
		{
			try
			{
				cols = Integer.parseInt(args[1]);
			}
			catch (Exception e)
			{
				// Leave height at the default
			}
		}

		Minesweeper game = new Minesweeper(rows, cols);

		Scanner sc = new Scanner(System.in);
		
		// Make a timer to exit the program if there's no input for 5 minutes.
		long timeoutTime = 1000*60*5;
		Timer t = new Timer();
		t.schedule(new TimerTask(){
			@Override
			public void run() {
				System.exit(0);
			}
		},timeoutTime,timeoutTime); 
		
		String s = sc.nextLine();
		while (!s.equals("exit") && !s.equals("quit"))
		{
			t.cancel();
			t.purge();
			t = new Timer();
			t.schedule(new TimerTask(){
				@Override
				public void run() {
					System.exit(0);
				}
			},timeoutTime,timeoutTime); 
			try
			{
				String[] params = s.split(" ");
				System.out.println(game.processCommand(params[0], params[1]));
			}
			catch (Exception e)
			{
				System.out.println(Minesweeper.RESULT_INVALID);
			}
			s = sc.nextLine();
		}
		
		System.exit(0);
	}
}
