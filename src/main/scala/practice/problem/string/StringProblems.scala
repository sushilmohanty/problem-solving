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
    val wordsArray = str.split(" ")
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
        // A - a = 34
        val charInLower = (charAsciiValue + 32).toChar
        outputStr.append(charInLower)
      } else {
        outputStr.append(char)
      }
    }
    outputStr.toString()
  }

}
