#include <iostream>
#include <random>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <bitset>
#include <map>

struct openKey {
public:

    int p;
    int h;

    std::vector<int> c;

};


struct closeKey {
public:

    int d;

    std::vector<int> f;
    std::vector<int> g;
    std::vector<int> pi;
    
};


unsigned long int modexp(unsigned long int x, 
    unsigned long int y, unsigned long int N)
{
    if (y == 0) return 1;
    unsigned long int z = modexp(x % N, y / 2, N) % N;
    if (y % 2 == 0)
        return (z * z) % N;
    else
        return ((x % N) * ((z * z) % N)) % N;
}


unsigned long int findPowerModSolution(int a, int b, int p) {
    for (auto i = 1; i <= p; i++) {
        //auto tmp = modexp(b, i, p);
        auto tmp = (b * i) % p;
        if (tmp == a)
            return i;
    }
    return -1;

}


void print(std::vector<int>& f)
{
    if (f.size() == 0) {
        std::cout << "Is empty!" << std::endl;
        return;
    }

    for (auto i = 0; i < f.size() - 1; i++) {
        std::cout << f[i] << "x^" << f.size() - i - 1 << " + ";
    }
    std::cout << f[f.size() - 1] << '\n';
}


void normalize(std::vector<int>& f)
{
    auto non_zero_index = 0;
    for (auto i = 0; i < f.size(); ++i) {
        if (f[i] != 0) {
            non_zero_index = i;
            break;
        }
    }
    f.erase(f.begin(), f.begin() + non_zero_index);
}

std::vector<int> add(const std::vector<int>& lhs,
    const std::vector<int>& rhs) {
    auto p = 7;
    auto lhsDegree = lhs.size();
    auto rhsDegree = rhs.size();
    auto degree = std::max(lhsDegree, rhsDegree);

    std::vector<int> copy(lhs);
    std::vector<int> copySub(rhs);

    std::vector<int> res(degree);

    //if (lhsDegree < rhsDegree)
    copy.insert(copy.begin(), degree - lhsDegree, 0);

    //if (rhsDegree < lhsDegree)
    copySub.insert(copySub.begin(), degree - rhsDegree, 0);


    for (auto i = 0; i < lhsDegree; i++) {
        auto tmp = (copy[i] + copySub[i]) % p;
        res[i] = tmp;
    }

    normalize(res);
    return res;
}


std::vector<int> sub(const std::vector<int>& lhs,
                    const std::vector<int>& rhs) {
    auto p = 7;
    auto lhsDegree = lhs.size();
    auto rhsDegree = rhs.size();
    auto degree = std::max(lhsDegree, rhsDegree);

    std::vector<int> copy(lhs);
    std::vector<int> copySub(rhs);

    std::vector<int> res(degree);

    //if (lhsDegree < rhsDegree)
    copy.insert(copy.begin(), degree - lhsDegree, 0);

    //if (rhsDegree < lhsDegree)
    copySub.insert(copySub.begin(), degree - rhsDegree, 0);

    for (auto i = 0; i < lhsDegree; i++) {
        auto tmp = copy[i] - copySub[i];
        if (tmp < 0) tmp += p;
        res[i] = tmp;
    }

    normalize(res);
    return res;
}


std::vector<int> mul(const std::vector<int>& lhs,
                    const std::vector<int>& rhs) {
    std::vector<int> res(lhs.size() + rhs.size() - 1, 0);
    for (auto i = 0; i < lhs.size(); i++) 
        for (auto j = 0; j < rhs.size(); j++)
            res[i + j] += lhs[i] * rhs[j];

    normalize(res);
    return res;
}


std::pair<std::vector<int>, std::vector<int>> div(
    const std::vector<int>& lhs,
    const std::vector<int>& rhs) {

    auto p = 7;

    if (lhs.size() < rhs.size()) {
        std::pair<std::vector<int>, std::vector<int>> res;
        std::vector<int> div(0);
        std::vector<int> rem(lhs);
        res.first = div;
        res.second = rem;
        return res;
    }


    std::vector<int> q(lhs);
    std::vector<int> r(0);

    std::map<int, int> result;
    
    while (q.size() >= rhs.size()) {
        auto diffDegree = q.size() - 1 - (rhs.size() - 1);
        auto coefficient = findPowerModSolution(q[0], rhs[0], p);

        std::vector<int> subtracted(rhs);
        std::transform(subtracted.begin(), subtracted.end(), subtracted.begin(), [coefficient, p](int xx) { return (coefficient * xx) % p; });
        subtracted.insert(subtracted.end(), diffDegree, 0);

        r = sub(q, subtracted);

        result[diffDegree] = coefficient;

        q = r;
    }


    auto maxKey = *std::max_element(result.begin(), result.end(), [](const auto& a, const auto& b) {
        return a.first < b.first;
        });

    std::vector<int> res(maxKey.first + 1, 0);
    for (const auto& element : result) {
        res[element.first] = element.second;
    }

    std::reverse(res.begin(), res.end());
    std::pair<std::vector<int>, std::vector<int>> answer(res, r);

    return answer;
}


unsigned long int solve(const std::vector<int>& f, int x) {
    std::vector<int> e = f;
    std::reverse(e.begin(), e.end());

    unsigned long int result = 0.0;
    auto power = 1;

    for (auto coefficient : e) {
        result += coefficient * power;
        power *= x;
    }

    return result;
}


unsigned long int factorial(int n) {
    return (n == 0) || (n == 1) ? 1 : n * factorial(n - 1);
}


int discreteLogarithm(std::vector<int> g, std::vector<int> f, int p, int q, int coef = 0) {
    std::vector<int> t(g);
    
    for (auto x = 2; true; x++) {
        t = mul(t, g);

        std::transform(t.begin(), t.end(), t.begin(), [p](int xx) { return  xx % p; });

        std::pair<std::vector<int>, std::vector<int>> d;
        d = div(t, f);
        t = d.second;

        auto tmp = solve(t, x);

        if (tmp == x + coef) {
            return x;
        }
            
    }
    return 0;
}



/// <summary>
/// 2.218
/// </summary>
/// <param name="g"></param>
/// <param name="h"></param>
/// <returns></returns>
std::vector<int> euclidean(std::vector<int>& g, std::vector<int>& h) {

    std::vector<int> gg(g);
    std::vector<int> hh(h);

    std::vector<int> r;

    while (hh.size() != 0) {

        auto d = div(gg, hh);
        r = d.second;

        gg = hh;
        hh = r;
    }

    return gg;
}


/// <summary>
/// 4.69
/// </summary>
/// <param name="f"></param>
/// <param name="p"></param>
/// <param name="h"></param>
/// <returns></returns>
bool isIrreducible(std::vector<int>& f, int p, int h) {

    std::vector<int> u;

    for (auto i = 1; i < h / 2; i++) {
        // 2.227 заменяю на мою прекрасную функцию умножения
        // что понимается под u(x) ?
    }
    return true;
}


/// <summary>
/// 4.70
/// </summary>
/// <param name="p"></param>
/// <param name="h"></param>
/// <returns></returns>
std::vector<int> monicIrreduciblePolynomial(const int p, const int h) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, p - 1);

    std::vector<int> irreducible(h, 1);

    do {

        for (auto i = 1; i < h; i++)
            irreducible[i] = dis(gen);

        while (irreducible[h - 1] == 0)
            irreducible[h - 1] = dis(gen);

    } while (!isIrreducible(irreducible, p, h));


    std::vector<int> f{ 1, 3, 5, 6, 2 };
    return f;
    //return irreducible;
}


int gcd(int a, int b) {
    while (b != 0) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}
std::vector<int> factorize(int a) {
    std::vector<int> factors;
    for (auto i = 2; i <= sqrt(a); ++i) {
        while (a % i == 0) {
            factors.push_back(i);
            a /= i;
        }
    }
    if (a > 1) {
        factors.push_back(a);
    }
    return factors;
}


/// <summary>
/// 4.80
/// </summary>
/// <param name="p"></param>
/// <returns></returns>
std::vector<int> findingGeneratorOfACyclicGroup(int p) {
    std::vector<int> fact;

    fact = factorize(p);

    std::vector<int> g{ 3, 3, 0, 6 };
    return g;

}


std::vector<int> permutation(int size) {
    
    std::random_device rd;
    std::mt19937 gen(rd());

    std::vector<int> p(size);
    std::iota(p.begin(), p.end(), 0);
    std::shuffle(p.begin(), p.end(), gen);

    std::vector<int> pp{ 6, 4, 0, 2, 1, 5, 3 };
    
    return pp;
}


int getD(int begin, int end) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(begin, end);

    return 1702;
    //return dis(gen);
}


std::pair<openKey, closeKey> keyGeneration(int p, int h) {

    int q = std::pow(p, h);
    
    std::vector<int> f;
    std::vector<int> g;

    f = monicIrreduciblePolynomial(p, h);
    g = findingGeneratorOfACyclicGroup(p);

    std::vector<int> a;

    for (auto x = 0; x < p; x++) 
        a.push_back(discreteLogarithm(g, f, p, q, x));

    std::vector<int> pi;
    pi = permutation(p);

    int d = getD(0, q - 2);

    std::vector<int> c(p);
    for (auto i = 0; i < p; i++) 
        c[i] = (a[pi[i]] + d) % (q - 1);
    
    openKey open;
    open.c = c;
    open.h = h;
    open.p = p;

    closeKey close;
    close.d = d;
    close.f = f;
    close.g = g;
    close.pi = pi;

    std::pair<openKey, closeKey> keys(open, close);
    return keys;
}


int binomialCoefficient(int p, int h) {
    return factorial(p) / (factorial(h) * factorial(p - h));
}


std::vector<int> getM(int m, int p, int h) {

    std::vector<int> M(p);
    auto l = h;
    
    for (auto i = 1; i <= p; i++) {

        auto tmp = 0;
        if (p != i && l == 0) {
            tmp = 1;
        }
        if (p > i) {
            tmp = binomialCoefficient(p - i, l); 
        }

        if (m >= tmp) {
            M[i - 1] = 1;
            m -= tmp;
            l -= 1;
        }
        else {
            M[i - 1] = 0;
        }

        if (l == 0 && tmp >= 0)
            M[i - 1] = 1;
        if (tmp == 0 && l >= 1)
            M[i - 1] = 0;

    }
    return M;
}


int encryption(openKey key, int m) {

    int binomial_coefficient = log2(binomialCoefficient(key.p, key.h));

    auto M = getM(m, key.p, key.h);

    int q = std::pow(key.p, key.h) - 1;

    int c = 0;
    for (auto i = 0; i < key.p; i++)
        c = (c + M[i] * key.c[i]) % q;

    return c;
}


int decryption(std::pair<openKey, closeKey> keys, int c) {

    auto open = keys.first;
    auto close = keys.second;

    auto p = open.p;

    int q = std::pow(open.p, open.h) - 1;
    auto r = (c - open.h * close.d) % q;
    if (r < 0) r += q;

    std::vector<int> u(close.g);

    for (auto i = 1; i < r; i++) {
        u = mul(u, close.g);
        std::transform(u.begin(), u.end(), u.begin(), [p](int xx) { return  xx % p; });
        auto tmp = div(u, close.f);
        u = tmp.second;
    }

    std::vector<int> s;
    s = add(u, close.f);

    print(s);
    std::cout << std::endl;


    int size = 0;
    std::cout << "Input solve" << std::endl;
    std::cin >> size;

    std::vector<int> t(size);
    for (auto i = 0; i < size; i++)
        std::cin >> t[i];

    std::vector<int> M(p, 0);
    for (auto i = 0; i < size; i++) {
        M[t[i]] = 1;
    }

    int m = 0;
    int l = open.h;
    for (auto i = 1; i < p; i++) {
        if (M[i-1] == 1) {
            m += binomialCoefficient(p - i, l);
            l--;
        }
           
    }

    return m;
}




int main()
{
    int p = 7;
    int h = 4;

    auto keys = keyGeneration(p, h);

    auto open = keys.first;
    auto close = keys.second;

    auto encryptionData = encryption(open, 22);

    std::cout << "Encryption data: " << encryptionData << std::endl;

    auto decryptionData = decryption(keys, encryptionData);

    std::cout << "\nDecryption data: " << decryptionData << std::endl;

    std::cout << "\n\nHello world :)";


}
