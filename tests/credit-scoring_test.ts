import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Can initialize user score",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const user1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('credit-scoring', 'initialize-user-score', [
                types.principal(user1.address)
            ], deployer.address)
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result, '(ok true)');
    }
});

Clarinet.test({
    name: "Can get credit score after initialization",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const user1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('credit-scoring', 'initialize-user-score', [
                types.principal(user1.address)
            ], deployer.address)
        ]);
        
        let creditScore = chain.callReadOnlyFn(
            'credit-scoring',
            'get-credit-score',
            [types.principal(user1.address)],
            deployer.address
        );
        
        creditScore.result.expectOk();
    }
});