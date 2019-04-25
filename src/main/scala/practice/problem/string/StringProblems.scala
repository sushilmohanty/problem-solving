package practice.problem.string

import scala.collection.mutable
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}


object StringProblems {

  /**
    * Given two strings check if one is rotation of other
    * eg : bar, arb, rba
    *
    * @param str1
    * @param str2
    * @return
    */

  def checkStringRotation(str1 : String, str2 : String) : Boolean = {
    // Concat both the strings
    // For a valid rotation second string must present as substring
    var isRotation = false
    if(str1.length == str2.length) {
      isRotation = (str1.concat(str2)).contains(str2)
    }
    isRotation
  }

  /**
    * Get max occurred char count in a string
    * Asumption : All chars are ASCII
    *
    * @param str
    * @return
    */

  def getMaxCharCountInString(str : String): Int = {
    val stringArray = str.toCharArray
    val lookupArray = Array.fill[Int](256)(0)
    var currMax = 0
    stringArray.foreach(char => {
      lookupArray(char.toInt) += 1
      if(currMax < lookupArray(char.toInt)) {
        currMax = lookupArray(char.toInt)
      }
    })
    currMax
  }


  /**
    * Remove duplicates from a String
    *
    * @param str
    * @return
    */
  def removeDuplicatesFromString(str : String): String = {
    val stringArray = str.toCharArray
    val set = new mutable.HashSet[Char]()
    val strDedupled = new mutable.StringBuilder()
    stringArray.foreach(char => {
      if (!set.contains(char)) {
        set.add(char)
        strDedupled.append(char)
      }
    })
    strDedupled.toString()
  }


  /**
    *
    * Check if a palindrome String can be constructed
    * using all the characters in the input String.
    *
    * @param str
    * @return
    */
  def checkPalindrome(str : String) : Boolean = {
    val stringArray = str.toCharArray
    val lookupArray = Array.fill[Int](256)(0)
    var oddOccourance = 0
    var isPalindrome = true
    stringArray.foreach(char => {
      lookupArray(char.toInt) += 1
    })
    breakable {
      lookupArray.foreach(element => {
        if (element == 1) oddOccourance += 1
        if (oddOccourance > 1 )  {
          isPalindrome = false
          break
        }
      })
    }
    return  isPalindrome
  }


  /**
    * Reverse words of a sentence
    *
    * @param str
    * @return
    */
  def reverseWordsOfSentence(str : String) : String = {
    val wordsArray = str.split("\\s+")
    val revString = new mutable.StringBuilder()
    for ( i <- wordsArray.length -1 to 0 by -1) {
      revString.append(wordsArray(i) + " ")
    }
    revString.toString().trim
  }


  /**
    * Reverse the chars of a String
    *
    * @param str
    * @return
    */
  def reverseCharsOfString(str : String) : String = {
    val charArray = str.toCharArray
    val revString = new mutable.StringBuilder()
    for (i <- charArray.length -1 to 0 by -1) {
      revString.append(charArray(i))
    }
    revString.toString()
  }


  /**
    * Get the first non repeating character
    *
    * @param str
    * @return
    */
  def getFirstNonRepeatingChar(str: String): Char = {
    val strArray = str.toCharArray
    val charMap = new mutable.LinkedHashMap[Char, Int]()
    var nonRepeatingChar = " ".charAt(0)
    strArray.foreach(char => {
      if (charMap.contains(char)) {
        val count = charMap.get(char).get
        charMap.put(char, count + 1)
      } else {
        charMap.put(char, 1)
      }
    })
    breakable {
      charMap.foreach(entry => {
        if (entry._2 == 1) {
          nonRepeatingChar = entry._1
          break
        }
      })
    }
    nonRepeatingChar
  }

  /**
    * Get run length encoding
    * eg : aaaabbccccdddzzz = a4b2c4d3z3
    *
    *
    * @param str
    * @return
    */
  def runLengthEncoding(str: String): String = {
    val charMap = new mutable.LinkedHashMap[Char, Int]()
    var outputStr = new mutable.StringBuilder()

    for (i <- 0 to str.length -1) {
      val ch = str.charAt(i)
      if (charMap.contains(ch)) {
        val count = charMap.get(ch).get
        charMap.put(ch, count + 1)
      } else {
        charMap.put(ch, 1)
      }
    }
    charMap.foreach(entry => {
      outputStr.append(entry._1).append(entry._2)
    })
    outputStr.toString
  }

  /**
    * implement toLowerCase
    *
    * @param str
    */
  def toLowerCase(str : String): String = {
    var outputStr = new mutable.StringBuilder()
    for (i <- 0 to str.length -1) {
      val char = str.charAt(i)
      val charAsciiValue = char.toInt
      // A = 65 and Z = 90
      if (charAsciiValue >= 65 && charAsciiValue <= 90) {
        // A - a = 32
        val charInLower = (charAsciiValue + 32).toChar
        outputStr.append(charInLower)
      } else {
        outputStr.append(char)
      }
    }
    outputStr.toString()
  }


  /**
    * Get no of unique email addresses
    *
    * @param emails
    * @return
    */
  def numUniqueEmails(emails: Array[String]): Int = {
    val uniqueSet = new mutable.HashSet[String]()
    emails.foreach(email => {
      var finalName = ""
      val domain = email.substring(email.indexOf("@"))
      if (email.contains("@")) {
        val name = email.substring(0, email.indexOf("@")).replaceAll("\\.", "")
        if (name.contains("+")) {
          finalName = name.substring(0, name.indexOf("+"))
        } else {
          finalName = name
        }
        println(finalName + domain)
        uniqueSet.add(finalName + domain)
      }
    })
      uniqueSet.size
  }


  /**
    * Find the smallest window in first String matching all characters in
    * second String.
    *
    * @param str1
    * @param str2
    * @return
    */
  def getSmallestWindowContainsAllChars(str1 : String, str2 : String) : String = {
    val smallLen = str2.length
    val lookupTable = new mutable.LinkedHashSet[String]()
    val smallestWinLen = 0
    // Get all substrings
    for(i <- 0 until  str1.length) {
      for(j <- i+1 to str1.length) {
        val subStr = str1.substring(i, j)
        // Consider substrings with length >= pattern
        if (subStr.length >= smallLen) {
          // Create a hashtable and add the stbstring elements
          // Check with matching string to ensure all chars present
          val substrArr = subStr.toCharArray
          var substrCharSet = new mutable.HashSet[Char]()
          substrCharSet = substrCharSet ++ substrArr
          var matchCount = 0;
          for (i <- 0 to str2.length -1) {
            if (substrCharSet.contains(str2(i))) {
              matchCount += 1
            }
          }
          // If all chars present match count will be equal to length
          if (matchCount == str2.length) {
            lookupTable.add(subStr)
          }

        }
      }
    }
    // From all eligible substrings find the smallest one
    var shortest = ""
    if (!lookupTable.isEmpty) {
      shortest = lookupTable.toList(0)
      lookupTable.foreach(element => {
        if (element.length < shortest.length) {
          shortest = element
        }
      })
    }
    shortest
  }

  /**
    * Check if two Strings are anagrams
    * All chars and their count must match
    *
    * eg : heater -> reheat
    *
    * @param str1
    * @param str2
    * @return
    */
  def checkAnagram(str1 : String, str2 : String) : Boolean = {
   val isAnagram = true
    if (str1.length != str2.length) {
      return false
    }
    val charTable = new mutable.HashMap[Char, Int]()
    // Iterate over the first array and populate hashtable
    for (i <- 0 to str1.length -1) {
      if (charTable.contains(str1(i))) {
        var count = charTable.get(str1(i)).get
        charTable.put(str1(i), count + 1)
      } else {
        charTable.put(str1(i), 1)
      }
    }
    // iterate over second String and substract count
    for (i <- 0 to str2.length -1) {
      if (charTable.contains(str2(i))) {
        var count = charTable.get(str1(i)).get
        charTable.put(str1(i), count - 1)
      }
    }
    // Check idf any count is non zero
    charTable.foreach(entry => {
      if (entry._2 != 0 ) {
        return false
      }
    })
    isAnagram
  }

}