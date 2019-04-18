package com.github.unchama.seichiassist.database.init;

import com.github.unchama.seichiassist.Config;
import com.github.unchama.seichiassist.database.DatabaseGateway;
import com.github.unchama.seichiassist.database.DatabaseConstants;
import com.github.unchama.seichiassist.database.init.ddl.*;
import com.github.unchama.util.ActionStatus;
import com.github.unchama.util.Try;
import com.github.unchama.util.ValuelessTry;
import org.apache.commons.lang3.tuple.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static com.github.unchama.util.ActionStatus.Fail;
import static com.github.unchama.util.ActionStatus.Ok;

public class DatabaseTableInitializer {
    private final DatabaseGateway gateway;
    private final Logger logger;
    private final Config config;

    public DatabaseTableInitializer(DatabaseGateway gateway, Logger logger, Config config) {
        this.gateway = gateway;
        this.logger = logger;
        this.config = config;
    }

    private String referenceFor(String tableName) {
        return this.gateway.databaseName + "." + tableName;
    }

    public ActionStatus initializeTables() {
        final Function<String, String> errorMessageForTable = (tableName) -> tableName + "テーブル作成に失敗しました";

        final String playerDataTableName = DatabaseConstants.PLAYERDATA_TABLENAME;
        final String gachaDataTableName = DatabaseConstants.GACHADATA_TABLENAME;
        final String mineStackGachaDataTableName = DatabaseConstants.MINESTACK_GACHADATA_TABLENAME;
        final String donateDataTableName = DatabaseConstants.DONATEDATA_TABLENAME;

        final Map<String, TableInitializationQueryGenerator> tableQueryGenerators =
                new HashMap<String, TableInitializationQueryGenerator>() {
                    {
                        put(
                                playerDataTableName,
                                new PlayerDataTableQueryGenerator(referenceFor(playerDataTableName), config)
                        );
                        put(
                                gachaDataTableName,
                                new GachaDataTableQueryGenerator(referenceFor(gachaDataTableName))
                        );
                        put(
                                mineStackGachaDataTableName,
                                new MineStackGachaDataTableQueryGenerator(referenceFor(mineStackGachaDataTableName))
                        );
                        put(
                                donateDataTableName,
                                new DonateDataTableQueryGenerator(referenceFor(donateDataTableName))
                        );
                    }
                };

        final Function<TableInitializationQueryGenerator, ActionStatus> initializeTable = (queryGenerator) ->
                ValuelessTry
                        .begin(() -> gateway.executeUpdate(queryGenerator.generateTableCreationQuery()))
                        .ifOkThen(() -> gateway.executeUpdate(queryGenerator.generateColumnCreationQuery()))
                        .overallStatus();

        final List<Pair<String, Supplier<ActionStatus>>> initializations =
                tableQueryGenerators.entrySet()
                        .stream()
                        .map((entry) -> {
                            final String errorMessage = errorMessageForTable.apply(entry.getKey());
                            final TableInitializationQueryGenerator queryGenerator = entry.getValue();
                            final Supplier<ActionStatus> initialization = () -> initializeTable.apply(queryGenerator);

                            return Pair.of(errorMessage, initialization);
                        })
                        .collect(Collectors.toList());

        return Try.sequentially(initializations)
                .mapFailValue(Ok, failedMessage -> { logger.info(failedMessage); return Fail; });
    }

}
