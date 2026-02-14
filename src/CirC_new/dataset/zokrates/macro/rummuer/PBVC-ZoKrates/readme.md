<H2>This is a part of paper <a href="https://www.sciencedirect.com/science/article/pii/S0167404819302494">Fair Payments for Verifiable Cloud Services</a> published at Computers and security journal</H2>
<p><H3>This code require the understanding of the <a href="https://blog.gnosis.pm/getting-started-with-zksnarks-zokrates-61e4f8e66bcc">ZoKrates tutorial</a></H3></p>
<ul><H2>Requirements</H2></ul>
<li>Node</li>
<li>npm</li>
<li>Truffle</li>
<li>Ganache</li>
<li>Meta-mask</li>
<ul><H2>Execution</H2></ul>
<li>Execute any file in the "code" folder by following instructions at <a href="https://blog.gnosis.pm/getting-started-with-zksnarks-zokrates-61e4f8e66bcc">ZoKrates tutorial</a></li>
<li>Copy the contract created using ZoKrates in Contracts/verifiable.sol</li>
<li>Copy the witness created using ZoKrates in to the arguments of src/app.js "VerifyTX" function</li>
<li>$npm install</li>
<li>Start Ganache</li>
<li>$truffle compile</li>
<li>$truffle migrate</li>
<li>$npm run dev</li>
<li>Connect meta-mask</li>
<li>Click the "call verifyTx" button and observe the output in the console</li>


