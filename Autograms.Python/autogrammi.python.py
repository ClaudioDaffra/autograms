incipit = "Counting the letters in this sentence there are"
findPangram = False           # If true, looks for a pangram instead of an autogram
upper_bound_limit = 15        # a good standard value is around 15 to 20.
first_iteration = 0           # change this value if sequential can't find anything, or if you want to restore a previous search
strategy = 'sequential'       # if sequential fails, try to explore with 'random': it can be very quick or it can go on forever. Try launching it again if you wait more than 5 minutes
terminator = 'and'            # if you can't find one with 'and', try '&', 'plus', etc...

import tensorflow as tf
import random
import re
import itertools
import math

def getFrequenciesVector(str):
  result = [0] * 26
  for char in str.lower():
    code = ord(char) - ord('a')
    if (0 <= code < 26):
      result[code] += 1

  return result
 
def getEnglishNumbers(maxLimit):
    numbers = {0: '', 1: 'one', 2: 'two', 3: 'three',
          4: 'four', 5: 'five', 6: 'six', 7: 'seven',
          8: 'eight', 9: 'nine', 10: 'ten', 11: 'eleven',
          12: 'twelve', 13: 'thirteen', 14: 'fourteen',
          15: 'fifteen', 16: 'sixteen', 17: 'seventeen',
          18: 'eighteen', 19: 'nineteen',
          20: 'twenty', 30: 'thirty', 40: 'forty', 50: 'fifty',
          60: 'sixty', 70: 'seventy', 80: 'eighty', 90: 'ninety'}

    for i in range(20, min(maxLimit, 100)):
        if (not i in numbers.keys()):
          numbers[i] = numbers[i - i % 10] + "-" + numbers[i % 10]

    return [numbers[i] + ('s' if i>1 else '') for i in sorted(numbers.keys())[:maxLimit]]
    
tBaseFrequency = tf.constant(getFrequenciesVector(incipit + " " + terminator))

minLimit = tf.tensor_scatter_nd_update(tf.where(tBaseFrequency > 0, 1, 0), indices=[[18]], updates=[1])
minLimit = tf.tensor_scatter_nd_update(minLimit, indices=[[18]], updates=[tf.reduce_sum(minLimit) + minLimit[18].numpy()])
minLimit = tBaseFrequency + minLimit

if findPangram:
  mask = tf.equal(minLimit, 0)
  minLimit = tf.where(mask, tf.ones_like(minLimit), minLimit)
  

tSumNumbersFrequencies = tf.reduce_sum(tEnglishNumberFrequencies, axis=0)

min_ranges = tf.where(tSumNumbersFrequencies > 0, 1, 0).numpy()
max_ranges = [upper_bound_limit if x/2 > upper_bound_limit else int(x/2) for x in tSumNumbersFrequencies.numpy()]

minimum_range = [max(0, minLimit.numpy()[i]) if min_ranges[i] == 0 else min_ranges[i] for i in range(26)]
maximum_range = [max(1, minLimit.numpy()[i]) if max_ranges[i] == 0 else max_ranges[i] for i in range(26)]

print("Lower bound: " + str(minimum_range))
print("Upper bound: " + str(maximum_range))

def getSentence(frequenciesVector):
  vector = [[chr(ord("a") + i),  re.sub("s$", "", englishNumbers[frequenciesVector[i]])]  for i in range(len(frequenciesVector))]
  vector = [v for v in vector if v[1] != '']
  result = incipit + " "
  for v in vector[:-1]:
    result += v[1] + " " + v[0] +  ("'s, " if v[1] != "one" else ', ')

  result = result[:-2] + " " + terminator + " " + vector[-1][1] + " " + vector[-1][0] + ("'s" if vector[-1][1] != "one" else '')

  return result
 
 
seenHashes = set()

def process_step(tensor_before):
  tensor_after = tBaseFrequency + tf.where(tensor_before > 0, 1, 0) + tf.reduce_sum(tf.gather(tEnglishNumberFrequencies, tensor_before), axis=0)
  difference = tf.reduce_sum(tf.math.abs(tensor_before - tensor_after)).numpy()

  return (tensor_after, difference)

tZeroes = tf.constant([0]*26)

def elaborate(vector):
  tGuess = tf.constant(vector)

  min_difference = 100000
  min_result = tZeroes

  hash_key = tGuess.numpy().tobytes()

  while not hash_key in seenHashes:
    seenHashes.add(hash_key)
    tGuess2 = tBaseFrequency + tf.where(tGuess > 0, 1, 0) + tf.reduce_sum(tf.gather(tEnglishNumberFrequencies, tGuess), axis=0)
    difference = tf.reduce_sum(tf.math.abs(tGuess - tGuess2)).numpy()
    if difference < min_difference:
      min_difference = difference
      min_result = tGuess

    tGuess = tGuess2
    hash_key = tGuess.numpy().tobytes()

  return [min_result, min_difference]
 
 
min_elab = [None, 1000]

def elab_combination(combination):
    global min_elab
    elab = elaborate(combination)
    if elab[1] < min_elab[1]:
      min_elab = elab
    return elab
    
import datetime

if strategy == 'sequential':
  start_time = datetime.datetime.now()

  seenSolutions = set()
  combinations = itertools.product(*[range(min_val, max_val + 1) for min_val, max_val in zip(minimum_range, maximum_range)])

  iterator = enumerate(combinations)

  for i in range(first_iteration):
    next(iterator)

  for [i,c] in iterator:
    best_elab = min_elab

    if (i%1000 == 0):
      print ("\n\nLooking for solutions in batch: " + str(i) + " - " + str(i + 1000) + " (" + str(len(seenHashes)) + " analyzed so far)")

    if (elab_combination(c)[1] <= best_elab[1] and str(min_elab[0]) not in seenSolutions):
      seenSolutions.add(str(min_elab[0]))
      best_coefficients = tuple(min_elab[0].numpy())
      print("|\n|-- New best sentence found at iteration: %s" % i)
      print("|   |-- Starting coefficients (before recursive processing):\t%s" % str(c))
      print("|   |-- Actual coefficients (after recursive processing):\t%s" % str(best_coefficients))

      if min_elab[1] > 0:
        print("|   |-- " + str(min_elab[1]) + " errors: " + getSentence(best_coefficients))
      else:
        print("\n\n****************************\n****  SOLUTION FOUND!   ****\n****************************\n")
        print("\n Found in %ss \n" % (datetime.datetime.now() - start_time))
        print(getSentence(best_coefficients))
        break
        
        
if strategy == 'random':
  [min_result, min_difference] = process_step( tf.random.uniform(shape=tf.shape(min_result), minval=0, maxval=6, dtype=tf.int32))
  last_min_difference =  100000

  i = 0

  while min_difference > 0:
    i += 1
    if (i % 100 == 0):
      print ("Trying %s: seen (%d)" % (list(min_result.numpy()), len(seenHashes)))
    if min_difference <= last_min_difference:
      last_min_difference = min_difference

      print("|\n|-- New best sentence found with %d errors" % min_difference)
      print("|   |-- Actual coefficients (after recursive processing):\t%s" % str(list(min_result.numpy())))


    min_result += tf.random.uniform(shape=tf.shape(min_result), minval=-6, maxval=6, dtype=tf.int32)
    min_result = tf.maximum(min_result, minLimit)


    [min_result, min_difference] = elaborate(min_result)

  print("\n\n****************************\n****  SOLUTION FOUND!   ****\n****************************\n")
  print(getSentence(min_result))
  
  
        

    
    