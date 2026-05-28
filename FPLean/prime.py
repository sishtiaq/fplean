
def is_prime(x):
    if x < 2:
        return False
    for i in range(2, int(x**0.5) + 1):
        if x % i == 0:
            return False
    return True

# A slow version of the prime checking function
def is_prime_slow(x):
    for i in range(2, x-1):
        if x % i == 0:
            # Found a divisor. Just return False immediately.
            return False 
    # Been through all possible divisors. No divisors found. So return True. 
    return True

def count_pulses(r, m, i):
    if m + i <= len(r):
        r_slice = r[m:m+i]
    else:
        r_ext = r * round((m+i) / len(r))
        r_slice = r_ext[m:m+i]

    # Count the number of pulses in the slice
    return sum(1 for x in r_slice if x > 0)


def group_by_first_letter(words):
    letter_to_words = {}
    for w in words:
        l = w[0]
        if l in letter_to_words:
            v = letter_to_words[l]
            print(f'v={v}, w={w}')
            new_v = v + [w]
            print(f'new_v={new_v}')
            letter_to_words[l] = new_v

        else:
            letter_to_words[l] = [w]
    return letter_to_words
 

def main():
    lst = ['Abeeha', 'Tariq', 'Ayesha', 'Samin']
    print(group_by_first_letter(lst))

if __name__ == "__main__":
    main()