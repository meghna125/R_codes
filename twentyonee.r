#Card game of 21



init<-function(PlayingCard)
{

      PlayingCard<-c("ace of spades","2 of spades","3 of spades",
               "4 of spades","5 of spades","6 of spades","7 of spades","8 of spades",
                "9 of spades","10 of spades","jack of spades","queen of spades","king of spades"
                 ,"ace of clubs","2 of clubs","3 of clubs",
               "4 of clubs","5 of clubs","6 of clubs","7 of clubs","8 of clubs",
                "9 of clubs","10 of clubs","jack of clubs","queen of clubs","king of clubs",
                 "ace of hearts","2 of hearts","3 of hearts",
               "4 of hearts","5 of hearts","6 of hearts","7 of hearts","8 of hearts",
                "9 of hearts","10 of hearts","jack of hearts","queen of hearts","king of hearts",
                 "ace of diamonds","2 of diamonds","3 of diamonds",
               "4 of diamonds","5 of diamonds","6 of diamonds","7 of diamonds","8 of diamonds",
                "9 of diamonds","10 of diamonds","jack of diamonds","queen of diamonds","king of diamonds")

     return(PlayingCard)

}

count<-function(PlayingCard)
{
   for(i in 1:52)
   {
     r[i]<-strsplit(PlayingCard[i], " ")[[1]][1]
   }
   return(r)    
}
                

SHUFFLE<-function(PlayingCard)
{
   
   for(i in 1:51)
   {
      j<-sample(i:52,1)
      temp=PlayingCard[i]
      PlayingCard[i]=PlayingCard[j]
      PlayingCard[j]=temp
   }
   return(PlayingCard)

}


DEAL<-function(PlayingCard)
{
    TopCard=PlayingCard[1]
    
    return(TopCard)
}

temp<-function(PlayingCard)
{
  
   for(i in 1:52)
   {
       PlayingCard[i]=PlayingCard[i+1]
   }
   return(PlayingCard)

}

RANK<-function(r,sum)
{
    if(r=="ace")
    {
        d<-sum+11
        if(d>21)
          return(1)
        else
          return(11)
    }
    else if(r=="jack")
      return(10)
    else if(r=="queen")
      return(10)
    else if(r=="king")
      return(10)
    else
      return(r)
}



Probability<-function(sum,len,c)
{
   
   probsum=0
   
   
     diff=21-sum
     if(diff==0)
       return(0)
   if(sum>=11)
   {
     for(i in 1:diff)
     {
         if(i==1)
         {
            
            cou<-length(which(c=="ace"))
            probsum<-probsum+cou

         }
         if(i==10)
         {
            c1<-length(which(c=="king"))
            c2<-length(which(c=="king"))
            c3<-length(which(c=="king"))
            c4<-length(which(c==i))
            probsum<-probsum+c1+c2+c3+c4
         }

         else 
         {
           cou<-length(which(c==i))
           probsum<-probsum+cou  
         }
       }

       return(probsum/len)
   }
   if(sum<11)
   {
     for(i in 2:diff)
     {
         if(i==11)
         {
            
            cou<-length(which(c=="ace"))
            probsum<-probsum+cou

         }
         if(i==10)
         {
            c1<-length(which(c=="king"))
            c2<-length(which(c=="king"))
            c3<-length(which(c=="king"))
            c4<-length(which(c==i))
            probsum<-probsum+c1+c2+c3+c4
         }
         if(i==2 | i==3 | i==4 |i==5 |i==6|i==7|i==8|i==9)
         {
           cou<-length(which(c==i))
           probsum<-probsum+cou  }
        }

        return(probsum/len)
     }

        
}


TwentyOne<-function()
{
    PlayingCard<-c()
    repeat
    {
        sum=0
        sumc=0
        flag=0
        flagc=0
      
        print("LETS PLAY 21!")
        print("SHUFFLING CARDS...")

        print("YOUR TURN")
         
        PlayingCard<-init(PlayingCard)      
        PlayingCard<-SHUFFLE(PlayingCard)
        topcard<-DEAL(PlayingCard)
        PlayingCard<-temp(PlayingCard)
        r<-strsplit(topcard, " ")[[1]][1]
        rank1<-RANK(r,sum)
        rank1<-as.numeric(rank1)
        sum<-sum+rank1

        print(topcard)
        print("SCORE")
        print(sum)
       
        topcard<-DEAL(PlayingCard)
        PlayingCard<-temp(PlayingCard)
        r<-strsplit(topcard, " ")[[1]][1]
        rank2<-RANK(r,sum)
        rank2<-as.numeric(rank2)
        sum<-sum+rank2

        print(topcard)
        print("SCORE")
        print(sum)

        if(sum>21)
        {
            print("YOU LOSE!")
            inp=readline(prompt=" PLAY AGAIN?(Y or N) ")
            if(inp=="N") 
            {
              break
            }
            next
        }


        len<-length(PlayingCard[!is.na(PlayingCard)])
        c<-count(PlayingCard)
        
        prob<-Probability(sum,len,c) 
        
        print("probability of getting 21 : ")
        print(prob)
       
        input<-readline(prompt=" HIT OR STAY? ")
        if(input=="HIT")
        {
           repeat
           {
             topcard<-DEAL(PlayingCard)
             PlayingCard<-temp(PlayingCard)
             r<-strsplit(topcard, " ")[[1]][1]
             rank<-RANK(r,sum)
             rank<-as.numeric(rank)
             sum<-sum+rank

             print(topcard)
             print("SCORE")
             print(sum)


           
             if(sum>21)
             {
               flag=1
               break 
             }
             
             len<-length(PlayingCard[!is.na(PlayingCard)])
             c<-count(PlayingCard)
        
             prob<-Probability(sum,len,c) 
        
             print("probability of getting 21 : ")
             print(prob)

              
            
             input<-readline(prompt=" HIT OR STAY? ")
             if(input=="STAY")
             {
                break
             }
             
           }
          
        }
         
        if(flag==1)
        {
           print("YOUR SCORE EXCEEDS 21")
           print("YOU LOSE!")
           inp=readline(prompt=" PLAY AGAIN?(Y or N) ")
           if(inp=="N") 
           {
             break
           }
           next
        }
     
        print("COMPUTER'S TURN")
        while(sumc<=17) 
        {
       
         
            PlayingCard<-init(PlayingCard)      
            PlayingCard<-SHUFFLE(PlayingCard)
            topcard<-DEAL(PlayingCard)
            PlayingCard<-temp(PlayingCard)
            r<-strsplit(topcard, " ")[[1]][1]
            rank<-RANK(r,sumc)
            rank<-as.numeric(rank)
            sumc<-sumc+rank

            print(topcard)
            print("SCORE")
            print(sumc)


        
            if(sumc>21)
            {
               flagc=1
               break
            }
       
             topcard<-DEAL(PlayingCard)
            PlayingCard<-temp(PlayingCard)
            r<-strsplit(topcard, " ")[[1]][1]
            rank<-RANK(r,sumc)
            rank<-as.numeric(rank)
            sumc<-sumc+rank

            print(topcard)
            print("SCORE")
            print(sumc)


            
            if(sumc>21)
            {
              flagc=1
              break
            }
            
        
        
       }
        
       if(flagc==1)
       {
          print("COMPUTER'S SCORE EXCEEDS 21")
          
          print("YOU WIN!")
          inp=readline(prompt=" PLAY AGAIN?(Y or N) ")
          if(inp=="N") 
          {
            break
          }
          next

       }
    
        
       print("YOUR SCORE : ")
       print(sum)
         
       print("COMPUTER'S SCORE : ")
       print(sumc)
         
       if(sum<=21 & sum>sumc)
          print("YOU WIN")
       if(sumc<=21 & sumc>=sum)
          print("YOU LOSE")
            

       inp=readline(prompt=" PLAY AGAIN?(Y or N) ")
       if(inp=="N" ) 
       {
            break
       }
       
    }    
    

}

 
TwentyOne()
          

        

