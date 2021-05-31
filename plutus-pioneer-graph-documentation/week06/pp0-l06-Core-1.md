# Contract: [Core.hs](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Core.hs) Layer1

### tag: [[pp0-l06-Core-1]]

---
This is the Plutus Pioneer first ever course. Thaught by Dr. Lars Brünjes. Find it on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g&t=4865s) and [Github](https://github.com/input-output-hk/plutus-pioneer-program).
---

---
** Note **
This is the first contract of Lecture 6. Refer to [[pp0-l06]]
---


- ON-CHAIN part
    - custom data type for `Oracle` [[pp00-l06-Core-1a]]
    - identify Oracle NFT Asset Class [[pp00-l06-Core-1b]]
    - get data from Datum [[pp00-l06-Core-1c]]
    - Oracle validator [[pp00-l06-Core-1d]], has two objectives:
        - 1) to check if the operator signed the transaction (as a hash)
        - 2) to check if the transaction carries Datum
    

- OFF-CHAIN part


