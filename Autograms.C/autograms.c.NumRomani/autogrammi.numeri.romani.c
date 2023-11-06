#include <stdio.h>
#include <string.h>

// i x v

// contatore massimo di loop per il conteggio lettere da 1 a 29

#define kar 30

// ar : indica il numero delle lettere presenti nel pangramma
//     : @ = 0 ovviamente non ci sono pangrammi con zero lettere dello stesso tipo

const char *ar[] =  // 0 1 -30
{
        "@"    
        ,"i"     ,"ii"      ,"iii"      ,"iv"   ,"v"    ,"vi"   ,"vii"      ,"viii"     ,"ix"       ,"x"
        ,"xi"    ,"xii"     ,"xiii"     ,"xiv"  ,"xv"   ,"xvi"  ,"xvii"     ,"xviii"    ,"xix"      ,"xx"
        ,"xxi"   ,"xxii"    ,"xxiii"    ,"xxiv" ,"xxv"  ,"xxvi" ,"xxvii"    ,"xxviii"   ,"xxix"     ,"xxx"  
} ;


// stringa iniziale del pangramma -> autogramma

const char *start = "abc";

// lettere dell'alfabeto (21 itliano) (26 inglese)

#define kAlfabeto 26

char alfabeto[kAlfabeto] =  
{
        0,0,0,0,0   , 0,0,0,0,0 ,
        0,0,0,0,0   , 0,0,0,0,0 ,
        0,0,0,0,0   , 0,
} ;

// stringa ove è presente il pangramma attuale ( caratteri )

char pangramma[512];

//  per tutte le lettere dell'alfabeto , va a vedere quante sono dell'array alfabeto
//  usato come loop e prende le stringhe in ar[] numeri romani e compone : pangramma

void componi_pangramma(void)
{
 char buffer[128];
 pangramma[0]=0;
 sprintf ( pangramma , "%s :",start ) ;
 for(int i =0;i<kAlfabeto;i++)
 {
     if (alfabeto[i  ]!=0)
     {
         char c='a'+i;
         sprintf(buffer, " %6s  %2c ; ",ar[alfabeto[i]],c ) ;
         strcat( pangramma,buffer );
     }
 }
}


void inzializza_alfabeto_con_valori_fissi_start(void)
{
        // per tutti i caratteri dell'alfabeto
        for(int kcar=0;kcar<kAlfabeto;kcar++)
        {
            int counter=0;          
            int check_car='a'+kcar;
            // per tutti i caratteri della stringa
            for(int strlung=0;strlung<strlen(start);strlung++)
            {            
                if( start[strlung]==check_car)
                {
                    ++counter;
                }
            }
            if (counter!=0) alfabeto[kcar]=counter+1; // tiene conto della a : e a quinti a : aa
        }
}

// visualizza il pangramma che deriva dai loop di I V X che sono poi le varianti

void visualizza_pangramma_parziale(void)
{
    componi_pangramma();
    printf("\n %s ", pangramma ) ;
}

// alfabeto di controllo

char check_alfabeto[kAlfabeto] =  
{
        0,0,0,0,0   , 0,0,0,0,0 ,
        0,0,0,0,0   , 0,0,0,0,0 ,
        0,0,0,0,0   , 0,
} ;

void check_pangramma(void)
{
        // per tutti i caratteri dell'alfabeto
        for(int kcar=0;kcar<kAlfabeto;kcar++)
        {
            int counter=0;          
            int check_car='a'+kcar;
            // per tutti i caratteri della stringa
            for(int strlung=0;strlung<strlen(pangramma);strlung++)
            {            
                if( pangramma[strlung]==check_car)
                {
                    ++counter;
                }
            }
            check_alfabeto[kcar]=counter;
     
        }
}

int confronta_alfabeti(void)
{
    check_pangramma(); // ricostruisci auto gramma
   
    for(int i=0;i<kAlfabeto;i++)
    {
        //visualizza_alfabeto();
        //visualizza_check_alfabeto();        
        if ( check_alfabeto[i] != alfabeto[i] ) return 0 ;
    }
    return 1 ;
}
// confrontto chiave
// abbiamo il primo alfabeto   che è formato dalle quantitò definite dal loop principale ( numeri)
// abbiamo il secondo alfabeto che è formato a partire dai caratteri in pangramma e ricostrutio con i numeri

int visualizza_alfabeto(void)
{
 
    printf("\n");
    for(int i = 0; i < kAlfabeto; i++ )
    {
        printf("[%02d]",alfabeto[i]);
    }
   
}
int visualizza_check_alfabeto(void)
{
   
    printf("\n");
    for(int i = 0; i < kAlfabeto; i++ )
    {
        printf("(%02d)",check_alfabeto[i]);
    }    
   
}

int main(void)
{
 inzializza_alfabeto_con_valori_fissi_start();
 
 visualizza_pangramma_parziale();
 
     // cicla solo per le posizioni variabili
 
        for(int ci=0;ci<kar;ci++)                   // 'i'
        {
            alfabeto['i'-'a']=ci;            
            for(int cv=0;cv<kar;cv++)               // 'v'
            {        
                alfabeto['v'-'a']=cv;
                for(int cx=0;cx<kar;cx++)           // 'x'
                {                
                    alfabeto['x'-'a']=cx;
                    {
                        visualizza_pangramma_parziale();
                           //  visualizza_alfabeto();
                           //visualizza_check_alfabeto();                        
                        if ( confronta_alfabeti() == 1 )
                        {
                            printf ("\nEUREKA !\n" );
                            visualizza_alfabeto();
                           visualizza_check_alfabeto();  
                            visualizza_pangramma_parziale();
                            return 0; }
                    }
                }
            }              
        }    
 return 0 ;    
}
