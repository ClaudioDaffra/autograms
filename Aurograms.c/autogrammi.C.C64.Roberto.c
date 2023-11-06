#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

const char *numbers[] = 
{
      "",             
      "one",          "twos",         "threes",       "fours",
      "fives",        "sixs",         "sevens",       "eights",
      "nines",        "tens",         "elevens",      "twelves",
      "thirteens",    "fourteens",    "fifteens",     "sixteens",
      "seventeens",   "eighteens",    "nineteens",    "twentys",
      "twentyones",   "twentytwos",   "twentythrees", "twentyfours",
      "twentyfives",  "twentysixs",   "twentysevens", "twentyeights",
      "twentynines",  "thirtys",      "thirtyones",   "thirtytwos",
      "thirtythrees", "thirtyfours",  "thirtyfives",  "thirtysixs",
      "thirtysevens", "thirtyeights", "thirtynines",  "fortys",
      "fortyones",    "fortytwos",    "fortythrees",  "fortyfours",
      "fortyfives",   "fortysixs",    "fortysevens",  "fortyeights",
      "fortynines",   "fiftys",       "fiftyones",    "fiftytwos",
      "fiftythrees",  "fiftyfours",   "fiftyfives",   "fiftysixs",
      "fiftysevens",  "fiftyeights",  "fiftynines",   "sixtys",
      "sixtyones",    "sixtytwos",    "sixtythrees",  "sixtyfours"
};

void computeCharFrequencies(const char *input, int *frequencies) 
{
  memset(frequencies, 0, 26 * sizeof(int));

  for (int i = 0; input[i] != '\0'; ++i) 
  {
    char c = tolower(input[i]); 

    if (c >= 'a' && c <= 'z') 
	{
      int index = c - 'a';
      ++frequencies[index];
    }
  }
}

void printSolution(const char* input, const char* terminator, int v[26]) 
{
  putchar(14);
  int count = 0;
  int current = 0;

  printf("[%s] : ", input);
  
  for (int i = 0; i < 26; ++i) 
  {
    if (v[i] > 0) 
	{
      ++count;
    }
  }
  
  for (int i = 0; current < count; ++i) 
  {
	if (v[i] > 0) 
	{
		if (v[i]==1)
		{
			printf("%s %c%s", numbers[v[i]], 'a' + i, v[i] > 1 ? "'s" : "");
		}
		else
		{
			char *s=(char*)numbers[v[i]];
			int size = strlen(s); //Total size of string
			if (s[size-1]=='s')  s[size-1] = '\0';
			printf("%s %c%s", s, 'a' + i, v[i] > 1 ? "'s" : "");
		}		 

		++current;

		printf("%s", current < count - 1 ? ", " : current == count - 1 ? terminator : ".");
	}
 }

 printf("\n");
   // fflush(stdout);
}

int main() 
{
  putchar(14);
  const char *input = "in this sentence there are";
  const char *terminator = " and ";
  int findPangram = 0;

  printf(findPangram ? 
            "finding a pangram for : '%s ...%s...'\n\n" : 
            "finding an autogram for : '%s ...%s...'\n\n", 
    input, 
    terminator
  );

  int baseFrequencies[26];  
  int bestErrors = 5;
  int candidates[26];
  int frequencies[26];
  int errors = 5;
 
  int inputAndTerminatorLength = strlen(input) + strlen(terminator);
  
  char *inputAndTerminator = (char *)malloc(inputAndTerminatorLength + 1);
  strcpy(inputAndTerminator, input);
  strcat(inputAndTerminator, terminator);
  
  computeCharFrequencies(inputAndTerminator, baseFrequencies);

  //srand(time(NULL));

  while (bestErrors > 0) 
  {
    //Generates a new set of 26 candidates
    for (int i=0; i<26; ++i) 
	{
      candidates[i] = rand() % 12 + (findPangram ? 1 : 0);
    }    

    //Tries a few times the recursive process: 
    // 1) calculate the actual frequencies from the candidate array
    // 2) calculate the distance between the frequencies and the candidates
    // 3) if the distance is 0: solution found!
    // 4) otherwise, the new frequencies obtained after the process become the new candidates
    // 5) goto 1
    
    for (int n=0; n<10; ++n) 
	{
      //Starts from the base frequencies
      memcpy(frequencies, baseFrequencies, sizeof(baseFrequencies));

      //Adds 1 for every candidate > 0, plus the letters present in the number strings
      for (int i=0; i<26; ++i) 
	  {
        if (candidates[i] > 0) 
		{
          ++frequencies[i];
        }

        for (const char* c = numbers[candidates[i]]; *c != '\0'; ++c) 
		{
          ++frequencies[*c - 'a'];
        }
      }

      //Computes the distance from the original candidates
      errors = 0;
      for (int i=0; i<26; ++i) 
	  {
        if (candidates[i] != frequencies[i]) 
		{
          ++errors;
        }
      }

      //Checks if it's a correct (or better) solution
      if (errors < bestErrors) 
	  {
        bestErrors = errors;
		
        printf("\n\nfound a sentence with %d errors\n", errors);
		
        printSolution(input, terminator, frequencies);
	
      }

      //The new vector becomes the new candidate vector
      memcpy(candidates, frequencies, sizeof(frequencies));
    }

    //The candidates were not good. Try again with a different set of random candidates
  }

  printf("\n\n******** soloution found *********\n\n");
  putchar(14);
}